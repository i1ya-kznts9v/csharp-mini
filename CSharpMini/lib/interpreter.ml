open Ast
open Parser
open Operators
open Helpers
open Hashtbl_impr

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let ( >> ) x f = x >>= fun _ -> f
  let return = Result.ok
  let error = Result.error
end

type key_t = string [@@deriving show {with_path= false}]

type constructor_t =
  { key: key_t
  ; arguments: (types * names) list
  ; call_constructor: expressions option
  ; body: statements }
[@@deriving show {with_path= false}]

type field_t =
  {field_type: types; key: key_t; is_const: bool; sub_tree: expressions option}
[@@deriving show {with_path= false}]

type method_t =
  { method_type: types
  ; is_abstract: bool
  ; is_virtual: bool
  ; is_override: bool
  ; arguments: (types * names) list
  ; key: key_t
  ; body: statements option
  ; is_overriden: bool }
[@@deriving show {with_path= false}]

type class_t =
  { this_key: key_t
  ; fields_table: (key_t, field_t) Hashtbl_impr.t
  ; methods_table: (key_t, method_t) Hashtbl_impr.t
  ; constructors_table: (key_t, constructor_t) Hashtbl_impr.t
  ; children_keys: key_t list
  ; is_abstract: bool
  ; is_sealed: bool
  ; parent_key: key_t option
  ; decl_tree: classes }
[@@deriving show {with_path= false}]

let convert_name_to_key = function Some (Name x) -> Some x | None -> None
let convert_table_to_seq = Hashtbl.to_seq_values

let seq_hd seq =
  match seq () with Seq.Nil -> raise Not_found | Seq.Cons (x, _) -> x

let get_type_list = List.map fst

let make_method_key method_name arguments =
  String.concat "" (method_name :: List.map show_types (get_type_list arguments))

let make_constructor_key constructor_name arguments =
  String.concat ""
    (constructor_name :: List.map show_types (get_type_list arguments))

let equals_key = make_method_key "Equals" [(TClass "Object", Name "obj")]
let to_string_key = make_method_key "ToString" []

module ClassLoader (M : MONADERROR) = struct
  open M
  open Class

  let is_public = List.mem Public
  let is_static = List.mem Static
  let is_abstract = List.mem Abstract
  let is_const = List.mem Const
  let is_virtual = List.mem Virtual
  let is_override = List.mem Override
  let is_sealed = List.mem Sealed

  let get_element ht key =
    match Hashtbl_impr.get_element_option ht key with
    | None -> error "No such element in table"
    | Some element -> return element

  let update_element ht old_key new_value =
    Hashtbl.replace ht old_key new_value ;
    return ht

  let rec list_map list action base =
    match list with
    | [] -> return base
    | x :: xs -> action x >> list_map xs action base

  let rec seq_iter seq action base =
    match seq () with
    | Seq.Nil -> return base
    | Seq.Cons (x, next) -> action x >> seq_iter next action base

  let prepare_object_class ht =
    let constructors_table = Hashtbl.create 32 in
    let fields_table = Hashtbl.create 32 in
    let methods_table = Hashtbl.create 32 in
    let equals =
      { method_type= TInt
      ; is_abstract= false
      ; is_virtual= true
      ; is_override= false
      ; arguments= [(TClass "Object", Name "obj")]
      ; key= equals_key
      ; is_overriden= false
      ; body=
          apply Statement.statement_block
            {|
        {
          if(this == obj) return 1;
          else return 0;
        }
      |}
      } in
    let to_string =
      { method_type= TString
      ; is_abstract= false
      ; is_virtual= true
      ; is_override= false
      ; arguments= []
      ; key= to_string_key
      ; is_overriden= false
      ; body=
          apply Statement.statement_block
            {|
        {
          return "Object";
        }
      |} } in
    let get_declaration_tree =
      match
        apply class_decl
          {|
                public class Object 
                {
                    public int Equals(Object obj) 
                    {
                      if (this == obj) return 1;
                      else return 0;
                    }
                          
                    public string ToString() 
                    {
                      return "Object";
                    }
                  }
|}
      with
      | Some decl_tree -> return decl_tree
      | None -> error "Error in parsing Object class" in
    get_declaration_tree
    >>= fun declaration_tree ->
    let object_hardcode =
      { this_key= "Object"
      ; fields_table
      ; methods_table
      ; constructors_table
      ; children_keys= []
      ; is_abstract= false
      ; is_sealed= false
      ; parent_key= None
      ; decl_tree= declaration_tree } in
    Hashtbl.add methods_table equals.key equals ;
    Hashtbl.add methods_table to_string.key to_string ;
    Hashtbl.add ht object_hardcode.this_key object_hardcode ;
    return ht

  let check_modifiers_element pair =
    match pair with
    | modifier_list, field_method_constructor -> (
      match field_method_constructor with
      | Method (_, _, _, _) when is_sealed modifier_list ->
          error "Methods cannot be sealed"
      | Method (_, _, _, _) when is_const modifier_list ->
          error "Methods cannot be const"
      | Method (_, _, _, _)
        when is_abstract modifier_list && is_virtual modifier_list ->
          error "Abstract methods are virtual by default"
      | Method (_, _, _, _)
        when is_abstract modifier_list && is_override modifier_list ->
          error "Abstract methods cannot be override"
      | Method (_, _, _, _)
        when is_virtual modifier_list && is_override modifier_list ->
          error "Virtual and override - mutually exclusive modifiers"
      | Method (TVoid, Name "Main", [], _)
        when is_static modifier_list
             && (not (is_abstract modifier_list))
             && (not (is_virtual modifier_list))
             && not (is_override modifier_list) ->
          return ()
      | Method (_, Name "Main", _, _) ->
          error "Only one Main method can be in the program"
      | Method (_, _, _, _) when is_static modifier_list ->
          error "Static methods other than Main are not supported"
      | Method (_, _, _, _) -> return ()
      | Field (_, _)
        when (not (is_static modifier_list))
             && (not (is_abstract modifier_list))
             && (not (is_virtual modifier_list))
             && not (is_override modifier_list) ->
          return ()
      | Field (_, _) -> error "Wrong field modifiers"
      | Constructor (_, _, _, _)
        when is_public modifier_list
             && (not (is_static modifier_list))
             && (not (is_abstract modifier_list))
             && (not (is_const modifier_list))
             && (not (is_virtual modifier_list))
             && not (is_override modifier_list) ->
          return ()
      | Constructor (_, _, _, _) -> error "Wrong constructor modifiers" )

  let check_modifiers_class = function
    | Class (modifier_list, _, _, _) when is_const modifier_list ->
        error "Classes cannot be const"
    | Class (modifier_list, _, _, _)
      when is_abstract modifier_list && is_sealed modifier_list ->
        error "Abstract and sealed - mutually exclusive modifiers"
    | Class (modifier_list, _, _, _)
      when (not (is_static modifier_list))
           && (not (is_virtual modifier_list))
           && not (is_override modifier_list) ->
        return ()
    | Class (_, _, _, _) -> error "Wrong class modifiers"

  let many_add_to_class_table class_list ht =
    let add_with_check ht key value error_message =
      match get_element_option ht key with
      | None -> Hashtbl.add ht key value ; return ht
      | Some _ -> error error_message in
    let add_to_class_table ht class_to_add =
      match class_to_add with
      | Class
          (class_modifier_list, Name this_key, class_parent, class_element_list)
        ->
          let methods_table = Hashtbl.create 1024 in
          let fields_table = Hashtbl.create 1024 in
          let constructors_table = Hashtbl.create 1024 in
          check_modifiers_class class_to_add
          >>
          let add_element : modifiers list * fields -> unit M.t =
           fun element ->
            match element with
            | field_modifier_list, Field (field_type, field_pairs) ->
                let rec adder = function
                  | [] -> return ()
                  | (Name key, sub_tree) :: tail ->
                      let is_const = is_const field_modifier_list in
                      let field_t = {field_type; key; is_const; sub_tree} in
                      add_with_check fields_table key field_t "Similar fields"
                      >> adder tail in
                check_modifiers_element element >> adder field_pairs
            | ( method_modifier_list
              , Method (method_type, Name name, arguments, body) ) ->
                let key = make_method_key name arguments in
                let is_abstract_class = is_abstract class_modifier_list in
                let is_abstract = is_abstract method_modifier_list in
                let is_abstract_body =
                  match is_abstract with
                  | true -> (
                    match is_abstract_class with
                    | false -> error "Abstract method in non-abstract class"
                    | true -> (
                      match body with
                      | Some _ -> error "Abstract method cannot have body"
                      | None -> return () ) )
                  | false -> (
                    match body with
                    | Some _ -> return ()
                    | None -> error "Body missing in non-abstract method" )
                in
                let is_sealed_class = is_sealed class_modifier_list in
                let is_virtual = is_virtual method_modifier_list in
                let virtual_in_body =
                  match is_virtual with
                  | true -> (
                    match is_sealed_class with
                    | true -> error "Virtual method cannot be in sealed class"
                    | false -> return () )
                  | false -> return () in
                let is_override = is_override method_modifier_list in
                let method_t =
                  { method_type
                  ; is_abstract
                  ; is_virtual
                  ; is_override
                  ; arguments
                  ; key
                  ; is_overriden= false
                  ; body } in
                check_modifiers_element element
                >> virtual_in_body >> is_abstract_body
                >> add_with_check methods_table key method_t
                     "Method with this type exists"
                >> return ()
            | _, Constructor (Name name, arguments, call_constructor, body) ->
                let constructor_key = make_constructor_key name arguments in
                let match_name_with_class =
                  if name = this_key then return ()
                  else error "Constructor name does not match class name" in
                let constructor_t =
                  {key= constructor_key; arguments; call_constructor; body}
                in
                match_name_with_class
                >> check_modifiers_element element
                >> add_with_check constructors_table constructor_key
                     constructor_t "Constructor with this type exists"
                >> return () in
          let add_parent parent =
            match parent with
            | None -> Some "Object"
            | Some _ -> convert_name_to_key parent in
          let is_abstract = is_abstract class_modifier_list in
          let is_static = is_static class_modifier_list in
          let is_sealed =
            match is_static with
            | true -> true
            | false -> is_sealed class_modifier_list in
          let parent_key = add_parent class_parent in
          let class_t =
            { this_key
            ; fields_table
            ; methods_table
            ; constructors_table
            ; children_keys= []
            ; is_abstract
            ; is_sealed
            ; parent_key
            ; decl_tree= class_to_add } in
          list_map class_element_list add_element ()
          >> add_with_check ht this_key class_t "This class already exists"
    in
    list_map class_list (add_to_class_table ht) ht

  let many_add_default_constructor ht =
    Hashtbl.iter
      (fun class_key class_t ->
        if Hashtbl.length class_t.constructors_table = 0 then
          let constructor_key = make_constructor_key class_key [] in
          let constructor_t =
            { key= constructor_key
            ; arguments= []
            ; call_constructor= None
            ; body= StatementBlock [] } in
          Hashtbl.add class_t.constructors_table class_key constructor_t)
      ht ;
    return ht

  let many_update_children_key ht =
    let update : class_t -> class_t M.t =
     fun children ->
      match children.parent_key with
      | None -> return children
      | Some parent_key -> (
          let parent = get_element_option ht parent_key in
          match parent with
          | None -> error "No parent class found"
          | Some parent when not parent.is_sealed ->
              let new_parent =
                { parent with
                  children_keys= children.this_key :: parent.children_keys }
              in
              update_element ht parent_key new_parent >> return new_parent
          | Some _ -> error "Sealed or static class cannot be inherited" ) in
    seq_iter (convert_table_to_seq ht) update ht

  (*Functions for implementing inheritance*)

  let many_field_inheritance parent children =
    let parent_field_inheritance : class_t -> field_t -> unit t =
     fun children parent_field ->
      match get_element_option children.fields_table parent_field.key with
      | None ->
          return
            (Hashtbl.add children.fields_table parent_field.key parent_field)
      | Some _ -> return () in
    seq_iter
      (convert_table_to_seq parent.fields_table)
      (parent_field_inheritance children)
      ()

  let is_base_method = function CallMethod (Base, _) -> true | _ -> false
  let is_this_method = function CallMethod (This, _) -> true | _ -> false

  let many_check_parent_constructor parent =
    let check_call_constructor constructor =
      match constructor.call_constructor with
      | Some call_constructor -> (
        match is_this_method call_constructor with
        | true -> return ()
        | false -> error "The called method must be a constructor of this class"
        )
      | None -> return () in
    seq_iter
      (convert_table_to_seq parent.constructors_table)
      check_call_constructor ()

  let many_check_children_constructor children =
    let check_call_constructor constructor =
      match constructor.call_constructor with
      | Some call_constructor -> (
        match
          is_this_method call_constructor || is_base_method call_constructor
        with
        | true -> return ()
        | false ->
            error
              "The called method must be a constructor of this or parent class"
        )
      | None -> return () in
    seq_iter
      (convert_table_to_seq children.constructors_table)
      check_call_constructor ()

  let many_method_inheritance parent children =
    let parent_method_inheritance : class_t -> method_t -> unit t =
     fun children parent_method ->
      match get_element_option children.methods_table parent_method.key with
      | None when parent_method.is_abstract ->
          if children.is_abstract then
            return
              (Hashtbl.add children.methods_table parent_method.key
                 parent_method)
          else error "Abstract method must be overriden"
      | None when not parent_method.is_abstract ->
          return
            (Hashtbl.add children.methods_table parent_method.key parent_method)
      | Some child_overriden_method ->
          Hashtbl.replace children.methods_table parent_method.key
            {child_overriden_method with is_overriden= true} ;
          return ()
      | _ -> return () in
    seq_iter
      (convert_table_to_seq parent.methods_table)
      (parent_method_inheritance children)
      ()

  let many_check_method_override parent children =
    let check_children_method_override : class_t -> method_t -> unit t =
     fun parent children_method ->
      match children_method.is_override with
      | false -> return ()
      | true -> (
        match get_element_option parent.methods_table children_method.key with
        | None -> error "Cannot override non-existent method in parent"
        | Some parent_method -> (
          match parent_method.is_virtual || parent_method.is_abstract with
          | true -> return ()
          | false ->
              error
                "Cannot override non-virtual or non-abstract method in parent" )
        ) in
    seq_iter
      (convert_table_to_seq children.methods_table)
      (check_children_method_override parent)
      ()

  let rec transfert ht parent children =
    many_field_inheritance parent children
    >> many_method_inheritance parent children
    >> many_check_method_override parent children
    >> many_check_parent_constructor parent
    >> many_check_children_constructor children
    >>= fun _ -> transfert_on_children ht children

  and transfert_on_children ht children =
    let childrens_of_children = children.children_keys in
    list_map childrens_of_children
      (fun children_children_key ->
        get_element ht children_children_key
        >>= fun children_of_children ->
        transfert ht children children_of_children)
      ()

  let inheritance ht =
    get_element ht "Object"
    >>= fun object_t ->
    let processing children_key =
      get_element ht children_key
      >>= fun children -> transfert ht object_t children in
    list_map object_t.children_keys processing ht

  let load class_list class_table =
    match class_list with
    | [] -> error "Syntax error or empty file"
    | _ ->
        prepare_object_class class_table
        >>= fun class_table_with_object ->
        many_add_to_class_table class_list class_table_with_object
        >>= fun class_table_with_classes ->
        many_add_default_constructor class_table_with_classes
        >>= fun class_table_without_inheritance ->
        many_update_children_key class_table_without_inheritance
        >>= fun class_table_ready_to_inheritance ->
        inheritance class_table_ready_to_inheritance
end

module Interpretation (M : MONADERROR) = struct
  open M

  type variable =
    { v_type: types
    ; v_key: key_t
    ; is_const: bool
    ; assignment_count: int
    ; v_value: values
    ; scope_level: int }
  [@@deriving show {with_path= false}]

  type signal = WasBreak | WasContinue | WasReturn | NoSignal
  [@@deriving show {with_path= false}]

  type context =
    { cur_object: object_references
    ; var_table: (key_t, variable) Hashtbl_impr.t
    ; last_expr_result: values
    ; runtime_signal: signal
    ; curr_method_type: types
    ; is_main_scope: bool
    ; nested_loops_cnt: int
    ; scope_level: int
    ; cur_constr_key: key_t option
    ; prev_context: context option
    ; obj_created_cnt: int
    ; is_creation: bool
    ; constr_affilation: key_t option }
  [@@deriving show {with_path= false}]

  let make_context cur_object var_table =
    return
      { cur_object
      ; var_table
      ; last_expr_result= VVoid
      ; runtime_signal= NoSignal
      ; curr_method_type= TVoid
      ; is_main_scope= true
      ; nested_loops_cnt= 0
      ; scope_level= 0
      ; cur_constr_key= None
      ; prev_context= None
      ; obj_created_cnt= 0
      ; is_creation= false
      ; constr_affilation= None }

  let rec mfold_left2 f acc l1 l2 =
    match (l1, l2) with
    | [], [] -> return acc
    | x :: xs, y :: ys -> f acc x y >>= fun res -> mfold_left2 f res xs ys
    | _, _ -> error "Wrong lists for fold_left2"

  let rec mfold_right f l acc =
    match l with
    | [] -> return acc
    | x :: xs -> mfold_right f xs acc >>= fun r -> f x r

  let rec mfold_left f acc l =
    match l with
    | [] -> return acc
    | x :: xs -> f acc x >>= fun r -> mfold_left f r xs

  let get_elem_if_present_m ht key =
    match Hashtbl_impr.get_element_option ht key with
    | None -> error "No such element in table"
    | Some el -> return el

  let obj_num obj =
    try get_object_number obj |> fun n -> return n
    with Invalid_argument m -> error m

  let find_class_with_main ht =
    Hashtbl_impr.filter ht (fun _ c -> Hashtbl.mem c.methods_table "Main")
    |> fun fht ->
    match Hashtbl.length fht with
    | 0 -> error "Must be one Main()"
    | 1 -> return (seq_hd (convert_table_to_seq fht))
    | _ -> error "Must be one Main()"

  let startswith test_str sub_str =
    if String.length sub_str > String.length test_str then false
    else
      let sub = String.sub test_str 0 (String.length sub_str) in
      String.equal sub sub_str

  let rec expr_type_check t_expr ctx class_table =
    match t_expr with
    | Add (left, right) -> (
        expr_type_check left ctx class_table
        >>= fun lt ->
        match lt with
        | TInt -> (
            expr_type_check right ctx class_table
            >>= fun rt ->
            match rt with
            | TInt -> return TInt
            | TString -> return TString
            | _ -> error "Wrong type: must be <int> or <string>" )
        | TString -> (
            expr_type_check right ctx class_table
            >>= fun rt ->
            match rt with
            | TInt | TString -> return TString
            | _ -> error "Wrong type: must be <int> or <string>" )
        | _ -> error "Wrong type: must be <int> or <string>" )
    | Sub (left, right)
     |Div (left, right)
     |Mod (left, right)
     |Mult (left, right) -> (
        expr_type_check left ctx class_table
        >>= fun lt ->
        match lt with
        | TInt -> (
            expr_type_check right ctx class_table
            >>= fun rt ->
            match rt with
            | TInt -> return TInt
            | _ -> error "Wrong type: must be <int>" )
        | _ -> error "Wrong type: must be <int>" )
    | PostDec value | PostInc value | PrefDec value | PrefInc value -> (
        expr_type_check value ctx class_table
        >>= fun vt ->
        match vt with
        | TInt -> return TInt
        | _ -> error "Wrong type: must be <int>" )
    | And (left, right) | Or (left, right) -> (
        expr_type_check left ctx class_table
        >>= fun lt ->
        match lt with
        | TBool -> (
            expr_type_check right ctx class_table
            >>= fun rt ->
            match rt with
            | TBool -> return TBool
            | _ -> error "Wrong type: must be <bool>" )
        | _ -> error "Wrong type: must be <bool>" )
    | Not value -> (
        expr_type_check value ctx class_table
        >>= fun vt ->
        match vt with
        | TBool -> return TBool
        | _ -> error "Wrong type: must be <bool>" )
    | Less (left, right)
     |More (left, right)
     |LessOrEqual (left, right)
     |MoreOrEqual (left, right) -> (
        expr_type_check left ctx class_table
        >>= fun lt ->
        match lt with
        | TInt -> (
            expr_type_check right ctx class_table
            >>= fun rt ->
            match rt with
            | TInt -> return TBool
            | _ -> error "Wrong type: must be <int>" )
        | _ -> error "Wrong type: must be <int>" )
    | Equal (left, right) | NotEqual (left, right) -> (
        expr_type_check left ctx class_table
        >>= fun lt ->
        match lt with
        | TInt -> (
            expr_type_check right ctx class_table
            >>= fun rt ->
            match rt with
            | TInt -> return TBool
            | _ -> error "Wrong type: must be <int>" )
        | TString -> (
            expr_type_check right ctx class_table
            >>= fun rt ->
            match rt with
            | TString -> return TBool
            | _ -> error "Wrong type: must be <string>" )
        | TBool -> (
            expr_type_check right ctx class_table
            >>= fun rt ->
            match rt with
            | TBool -> return TBool
            | _ -> error "Wrong type: must be <bool>" )
        | TArray arr_lt -> (
            expr_type_check right ctx class_table
            >>= fun rt ->
            match rt with
            | TArray arr_rt when arr_lt = arr_rt -> return TBool
            | _ -> error "Wrong type: must be <same_types[]>" )
        | TClass ls -> (
            expr_type_check right ctx class_table
            >>= fun rt ->
            match rt with
            | TClass rs when ls = rs -> return TBool
            | TClass "null" -> return TBool
            | _ -> error "Wrong class type" )
        | _ -> error "Wrong type in equals-expression" )
    | Null -> return (TClass "null")
    | This -> (
      match ctx.cur_object with
      | ObjectReference {class_key= k; _} -> return (TClass k)
      | NullObjectReference -> error "Current object is null in context" )
    | Base -> (
      match ctx.cur_object with
      | ObjectReference {class_key= k; _} ->
          get_elem_if_present_m class_table k
          >>= fun k_clr ->
          ( match k_clr.parent_key with
          | Some k -> return k
          | None -> error "The current class has no parent" )
          >>= fun par_k -> return (TClass par_k)
      | NullObjectReference -> error "Current object is null in context" )
    | CallMethod (Base, _) -> return TVoid
    | CallMethod (This, _) -> return TVoid
    | CallMethod (Identifier m_ident, args) ->
        let curr_obj_key =
          match ctx.cur_object with
          | NullObjectReference -> "null"
          | ObjectReference {class_key= key; _} -> key in
        get_elem_if_present_m class_table curr_obj_key
        >>= fun curr_class ->
        check_method curr_class m_ident args ctx class_table
        >>= fun mr -> return mr.method_type
    | AccessByPoint (obj_expr, Identifier f_key) -> (
        expr_type_check obj_expr ctx class_table
        >>= fun obj_c ->
        match obj_c with
        | TClass "null" -> error "NullReferenceException"
        | TClass obj_key -> (
            get_elem_if_present_m class_table obj_key
            >>= fun obj_class ->
            let var_field_o = get_element_option obj_class.fields_table f_key in
            match var_field_o with
            | None -> error "No such field in class"
            | Some var_field -> return var_field.field_type )
        | _ -> error "Wrong type: must be an object of some class" )
    | AccessByPoint (obj_expr, CallMethod (Identifier m_ident, args)) -> (
        expr_type_check obj_expr ctx class_table
        >>= fun obj_c ->
        match obj_c with
        | TClass "null" -> error "NullReferenceException"
        | TClass obj_key ->
            get_elem_if_present_m class_table obj_key
            >>= fun obj_class ->
            check_method obj_class m_ident args ctx class_table
            >>= fun m_r -> return m_r.method_type
        | _ -> error "Wrong type: must be an object of some class" )
    | ArrayAccess (arr_expr, index_expr) -> (
        expr_type_check index_expr ctx class_table
        >>= fun ind_t ->
        match ind_t with
        | TInt -> (
            expr_type_check arr_expr ctx class_table
            >>= fun arr_t ->
            match arr_t with
            | TArray t -> return t
            | _ -> error "Wrong type: must be <some_type[]>" )
        | _ -> error "Wrong type: index must be <int>" )
    | ArrayCreationWithSize (arr_type, size) -> (
        expr_type_check size ctx class_table
        >>= fun size_t ->
        match size_t with
        | TInt -> (
          match arr_type with
          | TVoid -> error "Wrong type: arrays cannot be <void[]>"
          | TArray _ ->
              error "Wrong type: multidimensional arrays are not supported"
          | _ -> return (TArray arr_type) )
        | _ -> error "Wrong type: size must be <int>" )
    | ArrayCreationWithElements (arr_type, elems) ->
        let rec process_list_el = function
          | [] -> return (TArray arr_type)
          | el :: els -> (
              expr_type_check el ctx class_table
              >>= fun el_type ->
              match arr_type with
              | TClass cleft -> (
                match el_type with
                | TClass "null" -> process_list_el els
                | TClass cright ->
                    check_classname_assign cleft cright class_table
                    >> process_list_el els
                | _ ->
                    error
                      "Wrong type: an element in an array must be the type of \
                       this array" )
              | _ ->
                  if arr_type = el_type then process_list_el els
                  else
                    error
                      "Wrong type: an element in an array must be the type of \
                       this array" ) in
        process_list_el elems
    | ClassCreation (Name class_name, args) -> (
      match get_element_option class_table class_name with
      | None -> error ("No such class implemented: " ^ class_name ^ "\n")
      | Some cl_elem -> (
        match args with
        | [] -> return (TClass class_name)
        | _ ->
            check_constructor cl_elem args ctx class_table
            >> return (TClass class_name) ) )
    | Identifier key -> (
        let var_o = get_element_option ctx.var_table key in
        match var_o with
        | None -> (
          match ctx.cur_object with
          | ObjectReference {field_references_table= ft; _} -> (
            match get_element_option ft key with
            | None -> error ("No such variable or field with this name : " ^ key)
            | Some fr -> return fr.field_type )
          | _ -> error "NullReferenceException" )
        | Some v -> return v.v_type )
    | Value value -> (
      match value with
      | VBool _ -> return TBool
      | VInt _ -> return TInt
      | VString _ -> return TString
      | VObjectReference NullObjectReference -> return (TClass "null")
      | VObjectReference (ObjectReference {class_key= ck; _}) ->
          return (TClass ck)
      | VArray NullArrayReference -> return (TArray TVoid)
      | VArray (ArrayReference {array_type= t; _}) -> return (TArray t)
      | _ -> error "Wrong constant value" )
    | Assign (left, right) -> (
        expr_type_check left ctx class_table
        >>= fun lt ->
        match lt with
        | TVoid -> error "Can't assign anything to <void>"
        | TClass cleft_key -> (
            expr_type_check right ctx class_table
            >>= fun rt ->
            match rt with
            | TClass "null" -> return (TClass cleft_key)
            | TClass cright_key ->
                check_classname_assign cleft_key cright_key class_table
            | _ -> error "Wrong assign types" )
        | TArray (TClass cleft_key) -> (
            expr_type_check right ctx class_table
            >>= fun rt ->
            match rt with
            | TArray (TClass cright_key) ->
                check_classname_assign cleft_key cright_key class_table
            | _ -> error "Wrong assign types" )
        | _ ->
            expr_type_check right ctx class_table
            >>= fun rt ->
            if lt = rt then return rt else error "Wrong assign types" )
    | _ -> error "Wrong expression"

  and check_classname_assign cleft_key cright_key class_table =
    let rec check_parent_tree key =
      get_elem_if_present_m class_table key
      >>= fun clr_by_key ->
      if clr_by_key.this_key = cleft_key then return (TClass cright_key)
      else
        match clr_by_key.parent_key with
        | None ->
            error "Cannot assign the most general type to the least general"
        | Some par_k -> check_parent_tree par_k in
    check_parent_tree cright_key

  and check_classname_bool cleft_key cright_key class_table =
    let rec check_parent_tree key =
      match get_element_option class_table key with
      | None -> false
      | Some clr_by_key -> (
          if clr_by_key.this_key = cleft_key then true
          else
            match clr_by_key.parent_key with
            | None -> false
            | Some par_k -> check_parent_tree par_k ) in
    check_parent_tree cright_key

  and check_method cl_r m_name expr_list check_ctx class_table =
    let check_type_m : int -> types -> key_t -> method_t -> bool =
     fun pos curr_type _ value ->
      match List.nth_opt value.arguments pos with
      | None -> false
      | Some (found_type, _) -> (
        match curr_type with
        | TClass "null" -> (
          match found_type with TClass _ -> true | _ -> false )
        | TClass curr_key -> (
          match found_type with
          | TClass found_key ->
              check_classname_bool found_key curr_key class_table
          | _ -> false )
        | _ -> found_type = curr_type ) in
    let rec helper_checker curr_ht pos e_list ctx =
      match Hashtbl.length curr_ht with
      | 0 -> error "No such method implemented"
      | other -> (
        match e_list with
        | [] -> (
          match other with
          | 1 -> return (seq_hd (convert_table_to_seq curr_ht))
          | _ -> error "Cannot resolve method" )
        | e :: es ->
            expr_type_check e ctx class_table
            >>= fun e_type ->
            helper_checker
              (Hashtbl_impr.filter curr_ht (check_type_m pos e_type))
              (pos + 1) es ctx ) in
    Hashtbl_impr.filter cl_r.methods_table (fun _ mr ->
        startswith mr.key m_name)
    |> fun filtered_by_name ->
    helper_checker
      (Hashtbl_impr.filter filtered_by_name (fun _ mr ->
           List.length mr.arguments = List.length expr_list))
      0 expr_list check_ctx

  and check_constructor cl_r expr_list check_ctx class_table =
    let check_type_c : int -> types -> key_t -> constructor_t -> bool =
     fun pos curr_type _ value ->
      match List.nth_opt value.arguments pos with
      | None -> false
      | Some (found_type, _) -> (
        match curr_type with
        | TClass "null" -> (
          match found_type with TClass _ -> true | _ -> false )
        | TClass curr_key -> (
          match found_type with
          | TClass found_key ->
              check_classname_bool found_key curr_key class_table
          | _ -> false )
        | _ -> found_type = curr_type ) in
    let rec helper_checker_c curr_ht pos e_list ctx =
      match Hashtbl.length curr_ht with
      | 0 -> error "No such constructor implemented"
      | other -> (
        match e_list with
        | [] -> (
          match other with
          | 1 -> return (seq_hd (convert_table_to_seq curr_ht))
          | _ -> error "Cannot resolve constructor" )
        | e :: es ->
            expr_type_check e ctx class_table
            >>= fun e_type ->
            helper_checker_c
              (Hashtbl_impr.filter curr_ht (check_type_c pos e_type))
              (pos + 1) es ctx ) in
    helper_checker_c
      (Hashtbl_impr.filter cl_r.constructors_table (fun _ cr ->
           List.length cr.arguments = List.length expr_list))
      0 expr_list check_ctx

  let get_obj_value = function
    | VObjectReference o -> o
    | _ -> NullObjectReference

  let make_list_of_elem el size =
    let rec helper acc curr =
      match curr with 0 -> acc | x -> helper (el :: acc) (x - 1) in
    helper [] size

  let inc_scope_level ctx = {ctx with scope_level= ctx.scope_level + 1}
  let dec_scope_level ctx = {ctx with scope_level= ctx.scope_level - 1}

  let check_assign_cnt_f : field_references -> unit M.t =
   fun fld ->
    match fld.assignments_count with
    | 0 -> return ()
    | _ when not fld.is_const -> return ()
    | _ -> error "Assignment to a constant field"

  let check_assign_cnt_v var =
    match var.assignment_count with
    | 0 -> return ()
    | _ when not var.is_const -> return ()
    | _ -> error "Assignment to a constant variable"

  let delete_scope_var : context -> context M.t =
   fun ctx ->
    let delete : key_t -> variable -> unit =
     fun key el ->
      if el.scope_level = ctx.scope_level then Hashtbl.remove ctx.var_table key
    in
    Hashtbl.iter delete ctx.var_table ;
    return ctx

  let is_good_for_stmt = function
    | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
     |CallMethod (_, _)
     |AccessByPoint (_, CallMethod (_, _))
     |Assign (_, _) ->
        true
    | _ -> false

  let rec eval_stmt stmt sctx class_table =
    match stmt with
    | StatementBlock st_list ->
        let rec helper_eval : statements list -> context -> context M.t =
         fun stl hctx ->
          match stl with
          | [] -> return hctx
          | st :: sts -> (
            match st with
            | (Break | Continue | Return _) when sts <> [] ->
                error "Statemets block contains unreachable code"
            | _
              when hctx.nested_loops_cnt >= 1 && hctx.runtime_signal = WasBreak
              ->
                return hctx
            | _
              when hctx.nested_loops_cnt >= 1
                   && hctx.runtime_signal = WasContinue ->
                return hctx
            | _ when hctx.runtime_signal = WasReturn -> return hctx
            | _ ->
                eval_stmt st hctx class_table
                >>= fun head_ctx -> helper_eval sts head_ctx ) in
        helper_eval st_list sctx
        >>= fun sbctx ->
        if sbctx.is_main_scope then return sbctx else delete_scope_var sbctx
    | While (bexpr, lstmt) -> (
        let was_main = sctx.is_main_scope in
        let rec loop s ctx =
          if ctx.runtime_signal = WasBreak then
            match s with
            | StatementBlock _ ->
                return
                  (dec_scope_level
                     { ctx with
                       runtime_signal= NoSignal
                     ; nested_loops_cnt= ctx.nested_loops_cnt - 1 })
            | _ ->
                return
                  { ctx with
                    runtime_signal= NoSignal
                  ; nested_loops_cnt= ctx.nested_loops_cnt - 1 }
          else
            eval_expr bexpr ctx class_table
            >>= fun bectx ->
            match bectx.last_expr_result with
            | VBool false -> (
              match s with
              | StatementBlock _ ->
                  return
                    (dec_scope_level
                       { bectx with
                         nested_loops_cnt= ctx.nested_loops_cnt - 1
                       ; is_main_scope= was_main })
              | _ ->
                  return {bectx with nested_loops_cnt= ctx.nested_loops_cnt - 1}
              )
            | VBool true -> (
                eval_stmt s bectx class_table
                >>= fun lctx ->
                match lctx.runtime_signal with
                | WasReturn -> return lctx
                | WasContinue -> loop s {lctx with runtime_signal= NoSignal}
                | _ -> loop s lctx )
            | _ -> error "Wrong expression type for loop <while> condition"
        in
        match lstmt with
        | StatementBlock _ ->
            loop lstmt
              (inc_scope_level
                 { sctx with
                   nested_loops_cnt= sctx.nested_loops_cnt + 1
                 ; is_main_scope= false })
        | _ -> loop lstmt {sctx with nested_loops_cnt= sctx.nested_loops_cnt + 1}
        )
    | Break ->
        if sctx.nested_loops_cnt <= 0 then error "No loop for <break>"
        else return {sctx with runtime_signal= WasBreak}
    | Continue ->
        if sctx.nested_loops_cnt <= 0 then error "No loop for <continue>"
        else return {sctx with runtime_signal= WasContinue}
    | If (bexpr, then_stmt, else_stmt_o) -> (
        eval_expr bexpr sctx class_table
        >>= fun bectx ->
        let was_main = bectx.is_main_scope in
        match bectx.last_expr_result with
        | VBool true -> (
          match then_stmt with
          | StatementBlock _ ->
              eval_stmt then_stmt
                (inc_scope_level {bectx with is_main_scope= false})
                class_table
              >>= fun tctx ->
              return (dec_scope_level {tctx with is_main_scope= was_main})
          | _ -> eval_stmt then_stmt bectx class_table )
        | VBool false -> (
          match else_stmt_o with
          | Some (StatementBlock _ as else_stmt) ->
              eval_stmt else_stmt
                (inc_scope_level {bectx with is_main_scope= false})
                class_table
              >>= fun ectx ->
              return (dec_scope_level {ectx with is_main_scope= was_main})
          | Some else_stmt -> eval_stmt else_stmt bectx class_table
          | None -> return sctx )
        | _ -> error "Wrong expression type in <if> condition" )
    | For (dec_stmt_o, bexpr_o, after_expr_list, body_stmt) ->
        let was_main = sctx.is_main_scope in
        ( match dec_stmt_o with
        | None -> return (inc_scope_level {sctx with is_main_scope= false})
        | Some dec_stmt ->
            eval_stmt dec_stmt
              (inc_scope_level {sctx with is_main_scope= false})
              class_table )
        >>= fun dec_ctx ->
        let rec loop bs afs ctx =
          if ctx.runtime_signal = WasBreak then
            delete_scope_var
              { ctx with
                runtime_signal= NoSignal
              ; nested_loops_cnt= ctx.nested_loops_cnt - 1
              ; scope_level= ctx.scope_level - 1
              ; is_main_scope= was_main }
          else
            ( match bexpr_o with
            | None -> return {ctx with last_expr_result= VBool true}
            | Some bexpr -> eval_expr bexpr ctx class_table )
            >>= fun bectx ->
            match bectx.last_expr_result with
            | VBool false ->
                delete_scope_var
                  { bectx with
                    nested_loops_cnt= bectx.nested_loops_cnt - 1
                  ; scope_level= bectx.scope_level - 1
                  ; is_main_scope= was_main }
            | VBool true -> (
                let rec eval_inc_expr_list e_list c =
                  match e_list with
                  | [] -> return c
                  | e :: es ->
                      if is_good_for_stmt e then
                        eval_expr e c class_table
                        >>= fun ehctx -> eval_inc_expr_list es ehctx
                      else error "Wrong expression in after body" in
                eval_stmt bs
                  {bectx with scope_level= dec_ctx.scope_level + 1}
                  class_table
                >>= fun bdctx ->
                match bdctx.runtime_signal with
                | WasReturn -> return {bdctx with is_main_scope= was_main}
                | WasContinue ->
                    eval_inc_expr_list afs bdctx
                    >>= fun after_ctx ->
                    loop bs afs {after_ctx with runtime_signal= NoSignal}
                | _ ->
                    eval_inc_expr_list afs bdctx
                    >>= fun after_ctx -> loop bs afs after_ctx )
            | _ -> error "Wrong expression type in loop <for> condition" in
        loop body_stmt after_expr_list
          {dec_ctx with nested_loops_cnt= sctx.nested_loops_cnt + 1}
    | Return None when sctx.curr_method_type = TVoid ->
        return {sctx with last_expr_result= VVoid; runtime_signal= WasReturn}
    | Return None -> error "Return value type mismatch"
    | Return (Some rexpr) ->
        expr_type_check rexpr sctx class_table
        >>= fun rexpr_type ->
        if rexpr_type <> sctx.curr_method_type then
          error "Return value type mismatch"
        else
          eval_expr rexpr sctx class_table
          >>= fun rectx -> return {rectx with runtime_signal= WasReturn}
    | Expression sexpr ->
        if is_good_for_stmt sexpr then
          eval_expr sexpr sctx class_table >>= fun ectx -> return ectx
        else error "Wrong expression in statement"
    | VariableDecl (modifier, vars_type, var_list) ->
        let is_const : modifiers option -> bool = function
          | Some Const -> true
          | _ -> false in
        let rec helper_vardec v_list vctx =
          match v_list with
          | [] -> return vctx
          | (Name name, var_expr_o) :: vs -> (
            match vctx.cur_object with
            | NullObjectReference ->
                error "Cannot assign value to variable of null-object"
            | ObjectReference {field_references_table= frt; _} ->
                ( if Hashtbl.mem vctx.var_table name || Hashtbl.mem frt name then
                  error "Variable with this name is already defined"
                else
                  match var_expr_o with
                  | None ->
                      Hashtbl.add vctx.var_table name
                        { v_type= vars_type
                        ; v_key= name
                        ; is_const= is_const modifier
                        ; assignment_count= 0
                        ; v_value= get_type_default_value vars_type
                        ; scope_level= vctx.scope_level } ;
                      return vctx
                  | Some var_expr -> (
                      expr_type_check var_expr vctx class_table
                      >>= fun var_expr_type ->
                      let add_var ve =
                        eval_expr ve vctx class_table
                        >>= fun vare_ctx ->
                        Hashtbl.add vare_ctx.var_table name
                          { v_type= var_expr_type
                          ; v_key= name
                          ; is_const= is_const modifier
                          ; assignment_count= 1
                          ; v_value= vare_ctx.last_expr_result
                          ; scope_level= vare_ctx.scope_level } ;
                        return vare_ctx in
                      match var_expr_type with
                      | TClass "null" -> (
                        match vars_type with
                        | TClass _ -> add_var var_expr
                        | _ -> error "Wrong assign type in declaration" )
                      | TClass cright -> (
                        match vars_type with
                        | TClass cleft ->
                            check_classname_assign cleft cright class_table
                            >>= fun _ -> add_var var_expr
                        | _ -> error "Wrong assign type in declaration" )
                      | TArray (TClass cright) -> (
                        match vars_type with
                        | TArray (TClass cleft) ->
                            check_classname_assign cleft cright class_table
                            >>= fun _ -> add_var var_expr
                        | _ -> error "Wrong assign type in declaration" )
                      | _ when var_expr_type = vars_type -> add_var var_expr
                      | _ ->
                          error
                            ( "Wrong value type for declared variable: "
                            ^ show_types var_expr_type ) ) )
                >>= fun head_ctx -> helper_vardec vs head_ctx ) in
        helper_vardec var_list sctx

  and eval_expr expr ectx class_table =
    let eval_e e_expr ctx =
      let eval_op left right op =
        eval_expr left ctx class_table
        >>= fun lctx ->
        eval_expr right lctx class_table
        >>= fun rctx ->
        let l_value = lctx.last_expr_result in
        let r_value = rctx.last_expr_result in
        try
          let new_value = op l_value r_value in
          return {rctx with last_expr_result= new_value}
        with
        | Invalid_argument m -> error m
        | Division_by_zero -> error "Division by zero" in
      let eval_un v_expr op =
        eval_expr v_expr ctx class_table
        >>= fun vctx ->
        let v = vctx.last_expr_result in
        try
          let new_v = op v in
          return {vctx with last_expr_result= new_v}
        with Invalid_argument m -> error m in
      match e_expr with
      | Add (left, right) -> eval_op left right ( ++ )
      | Sub (left, right) -> eval_op left right ( -- )
      | Mult (left, right) -> eval_op left right ( ** )
      | Div (left, right) -> eval_op left right ( // )
      | Mod (left, right) -> eval_op left right ( %% )
      | And (left, right) -> eval_op left right ( &&& )
      | Or (left, right) -> eval_op left right ( ||| )
      | Not bexp -> eval_un bexp ( !!! )
      | Less (left, right) -> eval_op left right ( <<< )
      | More (left, right) -> eval_op left right ( >>> )
      | LessOrEqual (left, right) -> eval_op left right ( <<== )
      | MoreOrEqual (left, right) -> eval_op left right ( >>== )
      | Equal (left, right) -> (
          expr_type_check left ctx class_table
          >>= fun l_type ->
          match l_type with
          | TClass l_key ->
              get_elem_if_present_m class_table l_key
              >>= fun left_clr ->
              get_elem_if_present_m left_clr.methods_table equals_key
              >>= fun left_eqr ->
              if left_eqr.is_overriden then
                eval_expr
                  (AccessByPoint
                     (left, CallMethod (Identifier "Equals", [right])))
                  ctx class_table
              else eval_op left right ( === )
          | _ -> eval_op left right ( === ) )
      | NotEqual (left, right) -> eval_op left right ( !=! )
      | Value v -> return {ctx with last_expr_result= v}
      | Identifier id -> (
        match get_element_option ctx.var_table id with
        | Some var_by_id -> return {ctx with last_expr_result= var_by_id.v_value}
        | None -> (
          try
            get_object_info ctx.cur_object
            |> fun (_, frt, _) ->
            match get_element_option frt id with
            | Some f -> return {ctx with last_expr_result= f.field_value}
            | None -> error "No such variable or field"
          with Invalid_argument m | Failure m -> error m ) )
      | Null ->
          return
            {ctx with last_expr_result= VObjectReference NullObjectReference}
      | CallMethod (This, args) ->
          ( match ctx.cur_constr_key with
          | None ->
              error "this(...) call must be in Constructor(...) : this(...)"
          | Some k -> return k )
          >>= fun external_constr_key ->
          let get_cur_class_key =
            match ctx.cur_object with
            | NullObjectReference -> error "NullReferenceException"
            | ObjectReference {class_key= key; _} -> return key in
          get_cur_class_key
          >>= fun curr_class_key ->
          get_elem_if_present_m class_table curr_class_key
          >>= fun cur_class_r ->
          check_constructor cur_class_r args ctx class_table
          >>= fun constr_r ->
          if constr_r.key = external_constr_key then
            error "Constructor recursion"
          else
            prepare_constructor_block constr_r.body cur_class_r
              constr_r.call_constructor
            >>= fun c_body ->
            ( try
                prepare_table_with_args_exn (Hashtbl.create 100) args
                  constr_r.arguments ctx class_table
              with Invalid_argument m -> error m )
            >>= fun (vt, vctx) ->
            eval_stmt c_body
              {vctx with var_table= vt; is_creation= true}
              class_table
            >>= fun res_ctx ->
            return
              { res_ctx with
                last_expr_result= VVoid
              ; var_table= ctx.var_table
              ; constr_affilation= ctx.constr_affilation
              ; is_creation= true }
      | CallMethod (Base, args) -> (
          ( match ctx.cur_constr_key with
          | None ->
              error "base(...) call must be in Constructor(...) : base(...)"
          | Some k -> return k )
          >>= fun _ ->
          ( match ctx.constr_affilation with
          | None ->
              error "base(...) call must be in Constructor(...) : base(...)"
          | Some c_aff -> return c_aff )
          >>= fun curr_class_key ->
          get_elem_if_present_m class_table curr_class_key
          >>= fun cur_class_r ->
          match cur_class_r.parent_key with
          | None -> error "Bad base(...) call usage: this class has no parent"
          | Some par_key ->
              get_elem_if_present_m class_table par_key
              >>= fun par_r ->
              check_constructor par_r args ctx class_table
              >>= fun constr_r ->
              prepare_constructor_block constr_r.body par_r
                constr_r.call_constructor
              >>= fun par_constr_body ->
              ( try
                  prepare_table_with_args_exn (Hashtbl.create 100) args
                    constr_r.arguments ctx class_table
                with Invalid_argument m -> error m )
              >>= fun (vt, vctx) ->
              eval_stmt par_constr_body
                { vctx with
                  var_table= vt
                ; is_creation= true
                ; constr_affilation= Some par_key
                ; cur_constr_key= Some constr_r.key }
                class_table
              >>= fun res_ctx ->
              return
                { res_ctx with
                  last_expr_result= VVoid
                ; var_table= ctx.var_table
                ; constr_affilation= ctx.constr_affilation
                ; is_creation= true } )
      | This ->
          return {ctx with last_expr_result= VObjectReference ctx.cur_object}
      | AccessByPoint (obj_expr, Identifier f_key) -> (
          eval_expr obj_expr ctx class_table
          >>= fun octx ->
          let obj = octx.last_expr_result in
          match obj with
          | VObjectReference (ObjectReference {field_references_table= frt; _})
            ->
              get_elem_if_present_m frt f_key
              >>= fun fld -> return {octx with last_expr_result= fld.field_value}
          | _ -> error "Cannot access field of non-object" )
      | AccessByPoint (obj_expr, CallMethod (Identifier m_name, args)) -> (
          eval_expr obj_expr ctx class_table
          >>= fun octx ->
          let obj = octx.last_expr_result in
          match obj with
          | VObjectReference NullObjectReference ->
              error "NullReferenceException"
          | VObjectReference
              (ObjectReference
                {class_key= cl_k; field_references_table= c_frt; number= c_n})
            -> (
            match get_element_option class_table cl_k with
            | None -> error "No such class to call method"
            | Some obj_class ->
                check_method obj_class m_name args octx class_table
                >>= fun mr ->
                ( match mr.body with
                | None -> error "Error: abstract class creation"
                | Some b -> return b )
                >>= fun m_body ->
                let new_var_table : (key_t, variable) Hashtbl_impr.t =
                  Hashtbl.create 100 in
                ( try
                    prepare_table_with_args_exn new_var_table args mr.arguments
                      octx class_table
                  with Invalid_argument m -> error m )
                >>= fun (new_vt, new_ctx) ->
                eval_stmt m_body
                  { cur_object=
                      ObjectReference
                        { class_key= cl_k
                        ; field_references_table= c_frt
                        ; number= c_n }
                  ; var_table= new_vt
                  ; last_expr_result= VVoid
                  ; runtime_signal= NoSignal
                  ; curr_method_type= mr.method_type
                  ; is_main_scope= false
                  ; nested_loops_cnt= 0
                  ; scope_level= 0
                  ; cur_constr_key= None
                  ; prev_context= Some ctx
                  ; obj_created_cnt= ctx.obj_created_cnt
                  ; is_creation= false
                  ; constr_affilation= None }
                  class_table
                >>= fun m_res_ctx ->
                return
                  { new_ctx with
                    last_expr_result=
                      ( if mr.method_type = TVoid then VVoid
                      else m_res_ctx.last_expr_result )
                  ; obj_created_cnt= m_res_ctx.obj_created_cnt
                  ; is_creation= false } )
          | _ -> error "Cannot access field of non-object" )
      | CallMethod (Identifier m, args) ->
          eval_expr
            (AccessByPoint (This, CallMethod (Identifier m, args)))
            ctx class_table
      | ArrayAccess (arr_expr, index_expr) -> (
          eval_expr arr_expr ctx class_table
          >>= fun arrctx ->
          eval_expr index_expr ctx class_table
          >>= fun indctx ->
          let arr_v = arrctx.last_expr_result in
          let ind_v = indctx.last_expr_result in
          match arr_v with
          | VArray (ArrayReference {array_values= a_values; _}) -> (
            match ind_v with
            | VInt i when i < 0 || i >= List.length a_values ->
                error "IndexOutOfRangeException"
            | VInt i ->
                return {indctx with last_expr_result= List.nth a_values i}
            | _ -> error "Index must be int" )
          | VArray NullArrayReference -> error "NullReferenceException"
          | _ -> error "Cannot access a non-array" )
      | ArrayCreationWithSize (arr_type, size_expr) -> (
          eval_expr size_expr ctx class_table
          >>= fun szctx ->
          let size_v = szctx.last_expr_result in
          let init_v = get_type_default_value arr_type in
          match size_v with
          | VInt size ->
              return
                { szctx with
                  last_expr_result=
                    VArray
                      (ArrayReference
                         { array_type= arr_type
                         ; array_values= make_list_of_elem init_v size
                         ; number= szctx.obj_created_cnt + 1 })
                ; obj_created_cnt= szctx.obj_created_cnt + 1 }
          | _ -> error "Size must be int" )
      | ArrayCreationWithElements (a_type, expr_list) ->
          let make_val_list ex_list fctx =
            mfold_left
              (fun (a_lst, hctx) e ->
                eval_expr e hctx class_table
                >>= fun ectx ->
                let head_val = ectx.last_expr_result in
                return (a_lst @ [head_val], ectx))
              ([], fctx) ex_list in
          make_val_list expr_list ctx
          >>= fun (values, r_ctx) ->
          return
            { ctx with
              last_expr_result=
                VArray
                  (ArrayReference
                     { array_type= a_type
                     ; array_values= values
                     ; number= r_ctx.obj_created_cnt + 1 })
            ; obj_created_cnt= r_ctx.obj_created_cnt + 1 }
      | ClassCreation (Name class_name, c_args) ->
          get_elem_if_present_m class_table class_name
          >>= fun obj_class ->
          if obj_class.is_abstract then
            error "This class is abstract: no object creation allowed"
          else
            check_constructor obj_class c_args ctx class_table
            >>= fun constr_r ->
            let rec init_object cl_r init_ctx =
              let field_tuples =
                get_variable_field_pairs_list_typed cl_r.decl_tree in
              let rec helper_init acc_ht help_ctx = function
                | [] -> return help_ctx
                | (curr_f_type, Name f_name, f_expr_o) :: tps ->
                    let is_const_field f_key =
                      get_elem_if_present_m obj_class.fields_table f_key
                      >>= fun test_field -> return test_field.is_const in
                    ( match f_expr_o with
                    | Some f_expr -> (
                        expr_type_check f_expr help_ctx class_table
                        >>= fun expr_type ->
                        is_const_field f_name
                        >>= fun is_const_f ->
                        let add_field fe =
                          eval_expr fe help_ctx class_table
                          >>= fun fe_ctx ->
                          Hashtbl.add acc_ht f_name
                            { key= f_name
                            ; field_type= curr_f_type
                            ; field_value= fe_ctx.last_expr_result
                            ; is_const= is_const_f
                            ; assignments_count= 1 } ;
                          return (fe_ctx, acc_ht) in
                        match expr_type with
                        | TClass "null" -> (
                          match curr_f_type with
                          | TClass _ -> add_field f_expr
                          | _ -> error "Wrong assign type in field declaration"
                          )
                        | TClass cright -> (
                          match curr_f_type with
                          | TClass cleft ->
                              check_classname_assign cleft cright class_table
                              >>= fun _ -> add_field f_expr
                          | _ -> error "Wrong assign type in field declaration"
                          )
                        | TArray (TClass cright) -> (
                          match curr_f_type with
                          | TArray (TClass cleft) ->
                              check_classname_assign cleft cright class_table
                              >>= fun _ -> add_field f_expr
                          | _ -> error "Wrong assign type in declaration" )
                        | _ when expr_type = curr_f_type -> add_field f_expr
                        | _ -> error "Wrong assign type in declaration" )
                    | None ->
                        is_const_field f_name
                        >>= fun is_const_f ->
                        Hashtbl.add acc_ht f_name
                          { key= f_name
                          ; field_type= curr_f_type
                          ; field_value= get_type_default_value curr_f_type
                          ; is_const= is_const_f
                          ; assignments_count= 0 } ;
                        return (help_ctx, acc_ht) )
                    >>= fun (head_ctx, head_ht) ->
                    obj_num head_ctx.cur_object
                    >>= fun num ->
                    helper_init head_ht
                      { head_ctx with
                        cur_object=
                          ObjectReference
                            { class_key= class_name
                            ; field_references_table= head_ht
                            ; number= num } }
                      tps in
              match cl_r.parent_key with
              | None -> helper_init (Hashtbl.create 100) init_ctx field_tuples
              | Some par_key ->
                  get_elem_if_present_m class_table par_key
                  >>= fun parent_r ->
                  init_object parent_r init_ctx
                  >>= fun par_ctx ->
                  helper_init
                    (get_object_fields par_ctx.cur_object)
                    par_ctx field_tuples in
            let new_object =
              ObjectReference
                { class_key= class_name
                ; field_references_table= Hashtbl.create 100
                ; number= ctx.obj_created_cnt + 1 } in
            init_object obj_class
              { cur_object= new_object
              ; var_table= Hashtbl.create 100
              ; last_expr_result= VVoid
              ; runtime_signal= NoSignal
              ; curr_method_type= TVoid
              ; is_main_scope= false
              ; nested_loops_cnt= 0
              ; scope_level= 0
              ; prev_context= Some ctx
              ; obj_created_cnt= ctx.obj_created_cnt + 1
              ; cur_constr_key= None
              ; is_creation= false
              ; constr_affilation= None }
            >>= fun initres_ctx ->
            let get_new_var_table =
              try
                prepare_table_with_args_exn (Hashtbl.create 100) c_args
                  constr_r.arguments ctx class_table
              with Invalid_argument m -> error m in
            get_new_var_table
            >>= fun (vt, _) ->
            prepare_constructor_block constr_r.body obj_class
              constr_r.call_constructor
            >>= fun c_body ->
            eval_stmt c_body
              { initres_ctx with
                var_table= vt
              ; is_creation= true
              ; is_main_scope= false
              ; constr_affilation= Some obj_class.this_key
              ; cur_constr_key= Some constr_r.key }
              class_table
            >>= fun c_ctx ->
            return
              { ctx with
                last_expr_result= VObjectReference c_ctx.cur_object
              ; runtime_signal= NoSignal
              ; obj_created_cnt= c_ctx.obj_created_cnt }
      | Assign (Identifier var_key, val_expr) ->
          eval_expr val_expr ctx class_table
          >>= fun val_evaled_ctx ->
          update_identifier_v var_key val_evaled_ctx.last_expr_result
            val_evaled_ctx
      | Assign (AccessByPoint (obj_expr, Identifier f_name), val_expr) ->
          eval_expr val_expr ctx class_table
          >>= fun val_evaled_ctx ->
          update_field_v obj_expr f_name val_evaled_ctx class_table
      | Assign (ArrayAccess (arr_expr, index_expr), val_expr) -> (
          eval_expr val_expr ctx class_table
          >>= fun val_evaled_ctx ->
          eval_expr arr_expr val_evaled_ctx class_table
          >>= fun arr_evaled_ctx ->
          eval_expr index_expr arr_evaled_ctx class_table
          >>= fun index_evaled_ctx ->
          match arr_evaled_ctx.last_expr_result with
          | VArray arr -> (
            match index_evaled_ctx.last_expr_result with
            | VInt i -> (
                let new_val = val_evaled_ctx.last_expr_result in
                try
                  update_array_state_exn arr i new_val index_evaled_ctx
                  |> fun _ ->
                  return {index_evaled_ctx with last_expr_result= new_val}
                with Invalid_argument m | Failure m -> error m )
            | _ -> error "Wrong type for array index" )
          | _ -> error "Wrong type for array asssignment" )
      | PostInc (AccessByPoint (obj_expr, Identifier f))
       |PrefInc (AccessByPoint (obj_expr, Identifier f)) ->
          eval_expr
            (Assign
               ( AccessByPoint (obj_expr, Identifier f)
               , Add (AccessByPoint (obj_expr, Identifier f), Value (VInt 1)) ))
            ctx class_table
      | PostInc (Identifier var_key) | PrefInc (Identifier var_key) ->
          eval_expr
            (Assign
               (Identifier var_key, Add (Identifier var_key, Value (VInt 1))))
            ctx class_table
      | PostInc (ArrayAccess (arr_expr, index_expr))
       |PrefInc (ArrayAccess (arr_expr, index_expr)) ->
          eval_expr
            (Assign
               ( ArrayAccess (arr_expr, index_expr)
               , Add (ArrayAccess (arr_expr, index_expr), Value (VInt 1)) ))
            ctx class_table
      | PostDec (AccessByPoint (obj_expr, Identifier f))
       |PrefDec (AccessByPoint (obj_expr, Identifier f)) ->
          eval_expr
            (Assign
               ( AccessByPoint (obj_expr, Identifier f)
               , Sub (AccessByPoint (obj_expr, Identifier f), Value (VInt 1)) ))
            ctx class_table
      | PostDec (Identifier var_key) | PrefDec (Identifier var_key) ->
          eval_expr
            (Assign
               (Identifier var_key, Sub (Identifier var_key, Value (VInt 1))))
            ctx class_table
      | PostDec (ArrayAccess (arr_expr, index_expr))
       |PrefDec (ArrayAccess (arr_expr, index_expr)) ->
          eval_expr
            (Assign
               ( ArrayAccess (arr_expr, index_expr)
               , Sub (ArrayAccess (arr_expr, index_expr), Value (VInt 1)) ))
            ctx class_table
      | _ -> error "Wrong expression construction" in
    expr_type_check expr ectx class_table >>= fun _ -> eval_e expr ectx

  and update_identifier_v var_key new_val val_evaled_ctx =
    if Hashtbl.mem val_evaled_ctx.var_table var_key then (
      get_elem_if_present_m val_evaled_ctx.var_table var_key
      >>= fun old_var ->
      check_assign_cnt_v old_var
      >>= fun _ ->
      Hashtbl.replace val_evaled_ctx.var_table var_key
        { old_var with
          v_value= new_val
        ; assignment_count= old_var.assignment_count + 1 } ;
      return val_evaled_ctx )
    else
      match val_evaled_ctx.cur_object with
      | NullObjectReference -> error "NullReferenceException"
      | ObjectReference {field_references_table= cur_frt; _} ->
          if Hashtbl.mem cur_frt var_key then
            get_elem_if_present_m cur_frt var_key
            >>= fun old_field ->
            check_assign_cnt_f old_field
            >>= fun _ ->
            if val_evaled_ctx.is_creation then
              Hashtbl.replace cur_frt var_key
                {old_field with field_value= val_evaled_ctx.last_expr_result}
              |> fun _ -> return val_evaled_ctx
            else
              try
                update_object_state_exn val_evaled_ctx.cur_object var_key
                  val_evaled_ctx.last_expr_result val_evaled_ctx
                |> fun _ -> return val_evaled_ctx
              with
              | Invalid_argument m -> error m
              | Not_found -> error "No such field"
          else error "No such variable"

  and update_field_v obj_expr f_name val_evaled_ctx class_table =
    eval_expr obj_expr val_evaled_ctx class_table
    >>= fun obj_evaled_ctx ->
    let obj_r = get_obj_value obj_evaled_ctx.last_expr_result in
    let new_val = val_evaled_ctx.last_expr_result in
    try
      get_object_info obj_r
      |> fun (_, frt, _) ->
      if Hashtbl.mem frt f_name then
        get_elem_if_present_m frt f_name
        >>= fun old_field ->
        check_assign_cnt_f old_field
        >>= fun _ ->
        if obj_evaled_ctx.is_creation then
          Hashtbl.replace frt f_name
            {old_field with field_value= val_evaled_ctx.last_expr_result}
          |> fun _ -> return obj_evaled_ctx
        else
          update_object_state_exn obj_r f_name new_val obj_evaled_ctx
          |> fun _ -> return obj_evaled_ctx
      else error "No such field in class"
    with
    | Invalid_argument m | Failure m -> error m
    | Not_found -> error "No such field"

  and update_array_state_exn arr index new_value update_ctx =
    let rec update_states f_ht i n_val a_n =
      Hashtbl.iter
        (fun _ field_ref ->
          match field_ref with
          | {key= f_key; field_value= f_val; _} -> (
            match f_val with
            | VArray
                (ArrayReference
                  {array_type= at; array_values= cur_values; number= cur_num})
              -> (
                if cur_num = a_n then
                  Hashtbl.replace f_ht f_key
                    { field_ref with
                      field_value=
                        update_array_value
                          (VArray
                             (ArrayReference
                                { array_type= at
                                ; array_values= cur_values
                                ; number= cur_num }))
                          i n_val } ;
                match at with
                | TClass _ ->
                    List.iter
                      (fun v ->
                        match v with
                        | VObjectReference
                            (ObjectReference {field_references_table= frt; _})
                          ->
                            update_states frt i n_val a_n
                        | _ -> ())
                      cur_values
                | _ -> () )
            | VObjectReference
                (ObjectReference {field_references_table= frt; _}) ->
                update_states frt i n_val a_n
            | _ -> () ))
        f_ht in
    let rec helper_update i n_val u_ctx a_n =
      Hashtbl.iter
        (fun v_key var ->
          match var.v_value with
          | VObjectReference (ObjectReference {field_references_table= frt; _})
            ->
              update_states frt i n_val a_n
          | VArray
              (ArrayReference
                {array_type= at; array_values= cur_values; number= cur_num})
            -> (
              if cur_num = a_n then
                Hashtbl.replace u_ctx.var_table v_key
                  { var with
                    v_value=
                      update_array_value
                        (VArray
                           (ArrayReference
                              { array_type= at
                              ; array_values= cur_values
                              ; number= cur_num }))
                        i n_val }
                |> fun () ->
                match at with
                | TClass _ ->
                    List.iter
                      (fun v ->
                        match v with
                        | VObjectReference
                            (ObjectReference {field_references_table= frt; _})
                          ->
                            update_states frt i n_val a_n
                        | _ -> ())
                      cur_values
                | _ -> () )
          | _ -> ())
        u_ctx.var_table
      |> fun () ->
      match u_ctx.prev_context with
      | None -> ()
      | Some prev_ctx -> helper_update i n_val prev_ctx a_n in
    get_array_info arr
    |> fun (_, _, a_number) -> helper_update index new_value update_ctx a_number

  and update_object_state_exn obj field_key new_value update_ctx =
    let rec update_states f_ht f_key n_val o_num assign_cnt =
      Hashtbl.iter
        (fun _ field_ref ->
          match field_ref with
          | {field_value= f_val; _} -> (
            match f_val with
            | VObjectReference
                (ObjectReference {field_references_table= frt; number= fnum; _})
              ->
                ( if fnum = o_num then
                  match get_element_option frt f_key with
                  | None -> raise Not_found
                  | Some old_field ->
                      Hashtbl.replace frt f_key
                        { old_field with
                          field_value= n_val
                        ; assignments_count= assign_cnt } ) ;
                update_states frt f_key n_val o_num assign_cnt
            | VArray
                (ArrayReference {array_type= TClass _; array_values= v_list; _})
              ->
                List.iter
                  (fun v ->
                    match v with
                    | VObjectReference
                        (ObjectReference
                          {field_references_table= frt; number= c_num; _}) ->
                        ( if c_num = o_num then
                          match get_element_option frt f_key with
                          | None -> raise Not_found
                          | Some old_field ->
                              Hashtbl.replace frt f_key
                                { old_field with
                                  field_value= n_val
                                ; assignments_count= assign_cnt } ) ;
                        update_states frt f_key n_val o_num assign_cnt
                    | _ -> ())
                  v_list
            | _ -> () ))
        f_ht in
    let rec helper_update f_key n_val u_ctx o_num assign_cnt =
      Hashtbl.iter
        (fun _ var ->
          match var.v_value with
          | VObjectReference
              (ObjectReference {field_references_table= frt; number= fnum; _})
            ->
              if o_num = fnum then (
                match get_element_option frt f_key with
                | None -> raise Not_found
                | Some old_field ->
                    Hashtbl.replace frt f_key
                      { old_field with
                        field_value= n_val
                      ; assignments_count= assign_cnt } ;
                    update_states frt f_key n_val o_num assign_cnt )
              else update_states frt f_key n_val o_num assign_cnt
          | VArray
              (ArrayReference {array_type= TClass _; array_values= v_list; _})
            ->
              List.iter
                (fun v ->
                  match v with
                  | VObjectReference
                      (ObjectReference
                        {field_references_table= frt; number= c_num; _}) ->
                      ( if c_num = o_num then
                        match get_element_option frt f_key with
                        | None -> raise Not_found
                        | Some old_field ->
                            Hashtbl.replace frt f_key
                              { old_field with
                                field_value= n_val
                              ; assignments_count= assign_cnt } )
                      |> fun () ->
                      update_states frt f_key n_val o_num assign_cnt
                  | _ -> ())
                v_list
          | _ -> ())
        u_ctx.var_table
      |> fun () ->
      match u_ctx.prev_context with
      | None -> ()
      | Some prev_ctx -> helper_update f_key n_val prev_ctx o_num assign_cnt
    in
    get_object_info obj
    |> fun (_, object_frt, object_number) ->
    ( match get_element_option object_frt field_key with
    | None -> raise Not_found
    | Some f -> f.assignments_count + 1 )
    |> fun assign_cnt ->
    helper_update field_key new_value update_ctx object_number assign_cnt

  and prepare_table_with_args_exn ht args_l m_arg_list pr_ctx class_table =
    mfold_left2
      (fun (h_ht, hctx) arg tn_pair ->
        match tn_pair with
        | head_type, Name head_name ->
            eval_expr arg hctx class_table
            >>= fun he_ctx ->
            Hashtbl.add h_ht head_name
              { v_type= head_type
              ; v_key= head_name
              ; is_const= false
              ; assignment_count= 1
              ; v_value= he_ctx.last_expr_result
              ; scope_level= 0 } ;
            return (h_ht, he_ctx))
      (ht, pr_ctx) args_l m_arg_list

  and prepare_constructor_block current_body current_class
      current_call_constructor =
    match current_body with
    | StatementBlock body -> (
      match (current_call_constructor, current_class.parent_key) with
      | Some (CallMethod (Base, _)), None ->
          error "base() call in constructor in not child class"
      | Some (CallMethod (Base, _) as call_constructor), Some _ ->
          return (StatementBlock (Expression call_constructor :: body))
      | Some (CallMethod (This, _) as call_constructor), _ ->
          return (StatementBlock (Expression call_constructor :: body))
      | None, _ -> return current_body
      | _ -> error "Incorrect constructor call in constructor" )
    | _ -> error "Must be statement block in constructor"

  let execute : (key_t, class_t) Hashtbl.t -> context M.t =
   fun ht ->
    find_class_with_main ht
    >>= fun cl ->
    make_context
      (ObjectReference
         { class_key= cl.this_key
         ; field_references_table= Hashtbl.create 10
         ; number= 0 })
      (Hashtbl.create 10)
    >>= fun ctx ->
    let main = Hashtbl.find cl.methods_table "Main" in
    match main.body with
    | None -> error "Main() method cannot be abstract"
    | Some body_main -> eval_stmt body_main ctx ht
end
