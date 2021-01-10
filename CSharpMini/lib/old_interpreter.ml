(*This version of the interpreter was used only for debugging, all its results are applied in the used version*)
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
          match parent_method.is_virtual with
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
    { variable_type: types
    ; variable_key: key_t
    ; is_const: bool
    ; assignments_count: int
    ; variable_value: values
    ; scope_level: int }
  [@@deriving show {with_path= false}]

  type flags = WasBreak | WasContinue | WasReturn | NoFlag
  [@@deriving show {with_path= false}]

  type context =
    { current_object: object_references
    ; variables_table: (key_t, variable) Hashtbl_impr.t
    ; last_expression_result: values
    ; runtime_flag: flags
    ; current_method_type: types
    ; is_main_scope: bool
    ; nested_loops_count: int
    ; scope_level: int
    ; current_constructor_key: key_t option
    ; previous_context: context option
    ; objects_created_count: int
    ; is_creation: bool
    ; constructor_affilation: key_t option }
  [@@deriving show {with_path= false}]

  let context_initialize current_object variables_table =
    return
      { current_object
      ; variables_table
      ; last_expression_result= VVoid
      ; runtime_flag= NoFlag
      ; current_method_type= TVoid
      ; is_main_scope= true
      ; nested_loops_count= 0
      ; scope_level= 0
      ; current_constructor_key= None
      ; previous_context= None
      ; objects_created_count= 0
      ; is_creation= false
      ; constructor_affilation= None }

  let rec fold_left2 func acc list1 list2 =
    match (list1, list2) with
    | [], [] -> return acc
    | x :: xs, y :: ys ->
        func acc x y >>= fun result -> fold_left2 func result xs ys
    | _, _ -> error "Wrong lists for fold_left2"

  let rec fold_right func list acc =
    match list with
    | [] -> return acc
    | x :: xs -> fold_right func xs acc >>= fun result -> func x result

  let rec fold_left func acc list =
    match list with
    | [] -> return acc
    | x :: xs -> func acc x >>= fun result -> fold_left func result xs

  let get_element ht key =
    match Hashtbl_impr.get_element_option ht key with
    | None -> error "No such element in table"
    | Some element -> return element

  let starts_with test_str sub_str =
    if String.length sub_str > String.length test_str then false
    else
      let sub = String.sub test_str 0 (String.length sub_str) in
      String.equal sub sub_str

  let object_number obj =
    try get_object_number obj |> fun num -> return num
    with Invalid_argument message -> error message

  let find_class_with_main ht =
    Hashtbl_impr.filter ht (fun _ c -> Hashtbl.mem c.methods_table "Main")
    |> fun fht ->
    match Hashtbl.length fht with
    | 0 -> error "Must be one Main()"
    | 1 -> return (seq_hd (convert_table_to_seq fht))
    | _ -> error "Must be one Main()"

  let rec expressions_type_check expression context class_table =
    match expression with
    | Add (left, right) -> (
        expressions_type_check left context class_table
        >>= fun left_type ->
        match left_type with
        | TInt -> (
            expressions_type_check right context class_table
            >>= fun right_type ->
            match right_type with
            | TInt -> return TInt
            | TString -> return TString
            | _ -> error "Wrong type: must be <int> or <string>" )
        | TString -> (
            expressions_type_check right context class_table
            >>= fun right_type ->
            match right_type with
            | TInt | TString -> return TString
            | _ -> error "Wrong type: must be <int> or <string>" )
        | _ -> error "Wrong type: must be <int> or <string>" )
    | Sub (left, right)
     |Div (left, right)
     |Mod (left, right)
     |Mult (left, right) -> (
        expressions_type_check left context class_table
        >>= fun left_type ->
        match left_type with
        | TInt -> (
            expressions_type_check right context class_table
            >>= fun right_type ->
            match right_type with
            | TInt -> return TInt
            | _ -> error "Wrong type: must be <int>" )
        | _ -> error "Wrong type: must be <int>" )
    | PostDec value | PostInc value | PrefDec value | PrefInc value -> (
        expressions_type_check value context class_table
        >>= fun value_type ->
        match value_type with
        | TInt -> return TInt
        | _ -> error "Wrong type: must be <int>" )
    | And (left, right) | Or (left, right) -> (
        expressions_type_check left context class_table
        >>= fun left_type ->
        match left_type with
        | TBool -> (
            expressions_type_check right context class_table
            >>= fun right_type ->
            match right_type with
            | TBool -> return TBool
            | _ -> error "Wrong type: must be <bool>" )
        | _ -> error "Wrong type: must be <bool>" )
    | Not value -> (
        expressions_type_check value context class_table
        >>= fun value_type ->
        match value_type with
        | TBool -> return TBool
        | _ -> error "Wrong type: must be <bool>" )
    | Less (left, right)
     |More (left, right)
     |LessOrEqual (left, right)
     |MoreOrEqual (left, right) -> (
        expressions_type_check left context class_table
        >>= fun left_type ->
        match left_type with
        | TInt -> (
            expressions_type_check right context class_table
            >>= fun right_type ->
            match right_type with
            | TInt -> return TBool
            | _ -> error "Wrong type: must be <int>" )
        | _ -> error "Wrong type: must be <int>" )
    | Equal (left, right) | NotEqual (left, right) -> (
        expressions_type_check left context class_table
        >>= fun left_type ->
        match left_type with
        | TInt -> (
            expressions_type_check right context class_table
            >>= fun right_type ->
            match right_type with
            | TInt -> return TBool
            | _ -> error "Wrong type: must be <int>" )
        | TString -> (
            expressions_type_check right context class_table
            >>= fun right_type ->
            match right_type with
            | TString -> return TBool
            | _ -> error "Wrong type: must be <string>" )
        | TBool -> (
            expressions_type_check right context class_table
            >>= fun right_type ->
            match right_type with
            | TBool -> return TBool
            | _ -> error "Wrong type: must be <bool>" )
        | TArray left_array_type -> (
            expressions_type_check right context class_table
            >>= fun right_type ->
            match right_type with
            | TArray right_array_type when left_array_type = right_array_type ->
                return TBool
            | _ -> error "Wrong type: must be <same_types[]>" )
        | TClass left_class_name -> (
            expressions_type_check right context class_table
            >>= fun right_type ->
            match right_type with
            | TClass right_class_name when left_class_name = right_class_name ->
                return TBool
            | TClass "null" -> return TBool
            | _ -> error "Wrong class type" )
        | _ -> error "Wrong type in equals-expression" )
    | Null -> return (TClass "null")
    | This -> (
      match context.current_object with
      | ObjectReference {class_key= key; _} -> return (TClass key)
      | NullObjectReference -> error "Current object is null in context" )
    | Base -> (
      match context.current_object with
      | ObjectReference {class_key= key; _} ->
          get_element class_table key
          >>= fun class_t ->
          ( match class_t.parent_key with
          | Some key -> return key
          | None -> error "The current class has no parent" )
          >>= fun parent_key -> return (TClass parent_key)
      | NullObjectReference -> error "Current object is null in context" )
    | CallMethod (Base, _) -> return TVoid
    | CallMethod (This, _) -> return TVoid
    | CallMethod (Identifier method_name, arguments) ->
        let current_object_key =
          match context.current_object with
          | NullObjectReference -> "null"
          | ObjectReference {class_key= key; _} -> key in
        get_element class_table current_object_key
        >>= fun current_class ->
        method_check current_class method_name arguments context class_table
        >>= fun method_t -> return method_t.method_type
    | AccessByPoint (object_expression, Identifier field_key) -> (
        expressions_type_check object_expression context class_table
        >>= fun object_type ->
        match object_type with
        | TClass "null" -> error "NullReferenceException"
        | TClass class_key -> (
            get_element class_table class_key
            >>= fun class_t ->
            let field_t = get_element_option class_t.fields_table field_key in
            match field_t with
            | None -> error "No such field in class"
            | Some field_t -> return field_t.field_type )
        | _ -> error "Wrong type: must be an object of some class" )
    | AccessByPoint
        (object_expression, CallMethod (Identifier method_name, arguments)) -> (
        expressions_type_check object_expression context class_table
        >>= fun object_type ->
        match object_type with
        | TClass "null" -> error "NullReferenceException"
        | TClass class_key ->
            get_element class_table class_key
            >>= fun class_t ->
            method_check class_t method_name arguments context class_table
            >>= fun method_t -> return method_t.method_type
        | _ -> error "Wrong type: must be an object of some class" )
    | ArrayAccess (array_expression, index_expression) -> (
        expressions_type_check index_expression context class_table
        >>= fun index_type ->
        match index_type with
        | TInt -> (
            expressions_type_check array_expression context class_table
            >>= fun array_type ->
            match array_type with
            | TArray array_type -> return array_type
            | _ -> error "Wrong type: must be <some_type[]>" )
        | _ -> error "Wrong type: index must be <int>" )
    | ArrayCreationWithSize (array_type, size) -> (
        expressions_type_check size context class_table
        >>= fun size_type ->
        match size_type with
        | TInt -> (
          match array_type with
          | TVoid -> error "Wrong type: arrays cannot be <void[]>"
          | TArray _ ->
              error "Wrong type: multidimensional arrays are not supported"
          | _ -> return (TArray array_type) )
        | _ -> error "Wrong type: size must be <int>" )
    | ArrayCreationWithElements (array_type, elements) ->
        let rec elements_check = function
          | [] -> return (TArray array_type)
          | element :: tail -> (
              expressions_type_check element context class_table
              >>= fun element_type ->
              match array_type with
              | TClass array_class_name -> (
                match element_type with
                | TClass "null" -> elements_check tail
                | TClass element_class_name ->
                    class_assign_check array_class_name element_class_name
                      class_table
                    >> elements_check tail
                | _ ->
                    error
                      "Wrong type: an element in an array must be the type of \
                       this array" )
              | _ ->
                  if array_type = element_type then elements_check tail
                  else
                    error
                      "Wrong type: an element in an array must be the type of \
                       this array" ) in
        elements_check elements
    | ClassCreation (Name class_name, arguments) -> (
        let class_t = get_element_option class_table class_name in
        match class_t with
        | None -> error ("No such class implemented: " ^ class_name ^ "\n")
        | Some class_t -> (
          match arguments with
          | [] -> return (TClass class_name)
          | _ ->
              constructor_check class_t arguments context class_table
              >> return (TClass class_name) ) )
    | Identifier key -> (
        let variable = get_element_option context.variables_table key in
        match variable with
        | None -> (
          match context.current_object with
          | ObjectReference {field_references_table= field_table; _} -> (
              let field_t = get_element_option field_table key in
              match field_t with
              | None -> error "No such variable or field"
              | Some field_t -> return field_t.field_type )
          | NullObjectReference -> error "NullReferenceException" )
        | Some variable -> return variable.variable_type )
    | Value value -> (
      match value with
      | VBool _ -> return TBool
      | VInt _ -> return TInt
      | VString _ -> return TString
      | VObjectReference NullObjectReference -> return (TClass "null")
      | VObjectReference (ObjectReference {class_key= key; _}) ->
          return (TClass key)
      | VArray NullArrayReference -> return (TArray TVoid)
      | VArray (ArrayReference {array_type= arr_type; _}) ->
          return (TArray arr_type)
      | _ -> error "Wrong constant value" )
    | Assign (left, right) -> (
        expressions_type_check left context class_table
        >>= fun left_type ->
        match left_type with
        | TVoid -> error "Can't assign anything to <void>"
        | TClass left_class_key -> (
            expressions_type_check right context class_table
            >>= fun right_type ->
            match right_type with
            | TClass "null" -> return (TClass left_class_key)
            | TClass right_class_key ->
                class_assign_check left_class_key right_class_key class_table
            | _ -> error "Wrong assign types" )
        | TArray (TClass left_class_key) -> (
            expressions_type_check right context class_table
            >>= fun right_type ->
            match right_type with
            | TArray (TClass right_class_key) ->
                class_assign_check left_class_key right_class_key class_table
            | _ -> error "Wrong assign types" )
        | _ ->
            expressions_type_check right context class_table
            >>= fun right_type ->
            if left_type = right_type then
              let _ = print_string "Type check" in
              return right_type
            else error "Wrong assign types" )
    | _ -> error "Wrong expression"

  and class_type_polymorphism_check left_class_name right_class_name class_table
      =
    let rec check_parents class_key =
      match get_element_option class_table class_key with
      | None -> false
      | Some class_t -> (
          if class_t.this_key = left_class_name then true
          else
            match class_t.parent_key with
            | None -> false
            | Some parent_key -> check_parents parent_key ) in
    check_parents right_class_name

  and class_assign_check left_class_key right_class_key class_table =
    match
      class_type_polymorphism_check left_class_key right_class_key class_table
    with
    | false -> error "Cannot assign the most general type to the least general"
    | true -> return (TClass right_class_key)

  and constructor_check class_t arguments context class_table =
    let type_check : int -> types -> key_t -> constructor_t -> bool =
     fun position argument_type _ constructor_t ->
      match List.nth_opt constructor_t.arguments position with
      | None -> false
      | Some (found_type, _) -> (
        match argument_type with
        | TClass "null" -> (
          match found_type with TClass _ -> true | _ -> false )
        | TClass class_key -> (
          match found_type with
          | TClass found_class_key ->
              class_type_polymorphism_check found_class_key class_key
                class_table
          | _ -> false )
        | _ -> found_type = argument_type ) in
    let rec helper ht position arguments context =
      match Hashtbl.length ht with
      | 0 -> error "No such constructor implemented"
      | other -> (
        match arguments with
        | [] -> (
          match other with
          | 1 -> return (seq_hd (convert_table_to_seq ht))
          | _ -> error "Cannot resolve constructor" )
        | argument :: tail ->
            expressions_type_check argument context class_table
            >>= fun argument_type ->
            helper
              (Hashtbl_impr.filter ht (type_check position argument_type))
              (position + 1) tail context ) in
    helper
      (Hashtbl_impr.filter class_t.constructors_table (fun _ constructor_t ->
           List.length constructor_t.arguments = List.length arguments))
      0 arguments context

  and method_check class_t method_name arguments context class_table =
    let type_check : int -> types -> key_t -> method_t -> bool =
     fun position argument_type _ method_t ->
      match List.nth_opt method_t.arguments position with
      | None -> false
      | Some (found_type, _) -> (
        match argument_type with
        | TClass "null" -> (
          match found_type with TClass _ -> true | _ -> false )
        | TClass class_key -> (
          match found_type with
          | TClass found_class_key ->
              class_type_polymorphism_check found_class_key class_key
                class_table
          | _ -> false )
        | _ -> found_type = argument_type ) in
    let rec helper ht_filtred_by_argument position arguments context =
      match Hashtbl.length ht_filtred_by_argument with
      | 0 -> error "No such method implemented"
      | other -> (
        match arguments with
        | [] -> (
          match other with
          | 1 -> return (seq_hd (convert_table_to_seq ht_filtred_by_argument))
          | _ -> error "Cannot resolve method" )
        | argument :: tail ->
            expressions_type_check argument context class_table
            >>= fun argument_type ->
            helper
              (Hashtbl_impr.filter ht_filtred_by_argument
                 (type_check position argument_type))
              (position + 1) tail context ) in
    Hashtbl_impr.filter class_t.methods_table (fun _ method_t ->
        starts_with method_t.key method_name)
    |> fun methods_table_filtred_by_name ->
    helper
      (Hashtbl_impr.filter methods_table_filtred_by_name (fun _ method_t ->
           List.length method_t.arguments = List.length arguments))
      0 arguments context

  let expression_in_statements = function
    | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
     |CallMethod (_, _)
     |AccessByPoint (_, CallMethod (_, _))
     |Assign (_, _) ->
        true
    | _ -> false

  let field_check_assign_count : field_references -> unit t =
   fun field ->
    match field.assignments_count with
    | 0 -> return ()
    | _ when not field.is_const -> return ()
    | _ -> error "Assignment to a constant field"

  let variable_check_assign_count variable =
    match variable.assignments_count with
    | 0 -> return ()
    | _ when not variable.is_const -> return ()
    | _ -> error "Assignment to a constant variable"

  let make_list_of_element element size =
    let rec helper list size =
      match size with 0 -> list | x -> helper (element :: list) (x - 1) in
    helper [] size

  let increment_scope_level context =
    {context with scope_level= context.scope_level + 1}

  let decrement_scope_level context =
    {context with scope_level= context.scope_level - 1}

  let delete_variable_from_scope : context -> context t =
   fun ctx ->
    let delete : key_t -> variable -> unit =
     fun key variable ->
      if variable.scope_level = ctx.scope_level then
        Hashtbl.remove ctx.variables_table key in
    Hashtbl.iter delete ctx.variables_table ;
    return ctx

  let rec interpret_statements statement context class_table =
    match statement with
    | StatementBlock statement_list ->
        let rec helper : statements list -> context -> context t =
         fun stat_list ctx ->
          match stat_list with
          | [] -> return ctx
          | stat :: tail -> (
            match stat with
            | (Break | Continue | Return _) when tail <> [] ->
                error "Statemets block contains unreachable code"
            | _ when ctx.nested_loops_count >= 1 && ctx.runtime_flag = WasBreak
              ->
                return ctx
            | _
              when ctx.nested_loops_count >= 1 && ctx.runtime_flag = WasContinue
              ->
                return ctx
            | _ when ctx.runtime_flag = WasReturn -> return ctx
            | _ ->
                interpret_statements stat ctx class_table
                >>= fun new_ctx -> helper tail new_ctx ) in
        helper statement_list context
        >>= fun ctx ->
        if ctx.is_main_scope then return ctx else delete_variable_from_scope ctx
    | While (expr, stat) -> (
        let was_main = context.is_main_scope in
        let rec loop st ctx =
          if ctx.runtime_flag = WasBreak then
            match st with
            | StatementBlock _ ->
                return
                  (decrement_scope_level
                     { ctx with
                       runtime_flag= NoFlag
                     ; nested_loops_count= ctx.nested_loops_count - 1 })
            | _ ->
                return
                  { ctx with
                    runtime_flag= NoFlag
                  ; nested_loops_count= ctx.nested_loops_count - 1 }
          else
            interpret_expressions expr ctx class_table
            >>= fun new_ctx ->
            match new_ctx.last_expression_result with
            | VBool false -> (
              match st with
              | StatementBlock _ ->
                  return
                    (decrement_scope_level
                       { new_ctx with
                         nested_loops_count= ctx.nested_loops_count - 1
                       ; is_main_scope= was_main })
              | _ ->
                  return
                    {new_ctx with nested_loops_count= ctx.nested_loops_count - 1}
              )
            | VBool true -> (
                interpret_statements st new_ctx class_table
                >>= fun new_new_ctx ->
                match new_new_ctx.runtime_flag with
                | WasReturn -> return new_new_ctx
                | WasContinue -> loop st {new_new_ctx with runtime_flag= NoFlag}
                | _ -> loop st new_new_ctx )
            | _ -> error "Wrong expression type for loop <while> condition"
        in
        match stat with
        | StatementBlock _ ->
            loop stat
              (increment_scope_level
                 { context with
                   nested_loops_count= context.nested_loops_count + 1
                 ; is_main_scope= false })
        | _ ->
            loop stat
              {context with nested_loops_count= context.nested_loops_count + 1}
        )
    | Break ->
        if context.nested_loops_count <= 0 then error "No loop for <break>"
        else return {context with runtime_flag= WasBreak}
    | Continue ->
        if context.nested_loops_count <= 0 then error "No loop for <continue>"
        else return {context with runtime_flag= WasContinue}
    | If (expr, stat_body, stat_else) -> (
        interpret_expressions expr context class_table
        >>= fun new_ctx ->
        let was_main = new_ctx.is_main_scope in
        match new_ctx.last_expression_result with
        | VBool true -> (
          match stat_body with
          | StatementBlock _ ->
              interpret_statements stat_body
                (increment_scope_level {new_ctx with is_main_scope= false})
                class_table
              >>= fun new_new_ctx ->
              return
                (decrement_scope_level
                   {new_new_ctx with is_main_scope= was_main})
          | _ -> interpret_statements stat_body new_ctx class_table )
        | VBool false -> (
          match stat_else with
          | Some (StatementBlock _ as st_else) ->
              interpret_statements st_else
                (increment_scope_level {new_ctx with is_main_scope= false})
                class_table
              >>= fun new_new_ctx ->
              return
                (decrement_scope_level
                   {new_new_ctx with is_main_scope= was_main})
          | Some st_else -> interpret_statements st_else new_ctx class_table
          | None -> return context )
        | _ -> error "Wrong expression type in <if> condition" )
    | For (decl_stat, cond_expr, after_expr, body_stat) ->
        let was_main = context.is_main_scope in
        ( match decl_stat with
        | None ->
            return (increment_scope_level {context with is_main_scope= false})
        | Some decl_st ->
            interpret_statements decl_st
              (increment_scope_level {context with is_main_scope= false})
              class_table )
        >>= fun decl_ctx ->
        let rec loop body_st after_ex ctx =
          if ctx.runtime_flag = WasBreak then
            delete_variable_from_scope
              { ctx with
                runtime_flag= NoFlag
              ; nested_loops_count= ctx.nested_loops_count - 1
              ; scope_level= ctx.scope_level - 1
              ; is_main_scope= was_main }
          else
            ( match cond_expr with
            | None -> return {ctx with last_expression_result= VBool true}
            | Some cond_ex -> interpret_expressions cond_ex ctx class_table )
            >>= fun cond_ctx ->
            match cond_ctx.last_expression_result with
            | VBool false ->
                delete_variable_from_scope
                  { cond_ctx with
                    nested_loops_count= cond_ctx.nested_loops_count - 1
                  ; scope_level= cond_ctx.scope_level - 1
                  ; is_main_scope= was_main }
            | VBool true -> (
                let rec interpret_after_ex expr_list cont =
                  match expr_list with
                  | [] -> return cont
                  | expr :: tail ->
                      if expression_in_statements expr then
                        interpret_expressions expr cont class_table
                        >>= fun new_ctx -> interpret_after_ex tail new_ctx
                      else error "Wrong expression in after body" in
                interpret_statements body_st
                  {cond_ctx with scope_level= decl_ctx.scope_level + 1}
                  class_table
                >>= fun body_ctx ->
                match body_ctx.runtime_flag with
                | WasReturn -> return {body_ctx with is_main_scope= was_main}
                | WasContinue ->
                    interpret_after_ex after_ex body_ctx
                    >>= fun after_ctx ->
                    loop body_st after_ex {after_ctx with runtime_flag= NoFlag}
                | _ ->
                    interpret_after_ex after_ex body_ctx
                    >>= fun after_ctx -> loop body_st after_ex after_ctx )
            | _ -> error "Wrong expression type in loop <for> condition" in
        loop body_stat after_expr
          {decl_ctx with nested_loops_count= context.nested_loops_count + 1}
    | Return None when context.current_method_type = TVoid ->
        return
          {context with last_expression_result= VVoid; runtime_flag= WasReturn}
    | Return None -> error "Return value type mismatch"
    | Return (Some ex) ->
        expressions_type_check ex context class_table
        >>= fun ex_type ->
        if ex_type <> context.current_method_type then
          error "Return value type mismatch"
        else
          interpret_expressions ex context class_table
          >>= fun new_ctx -> return {new_ctx with runtime_flag= WasReturn}
    | Expression expr ->
        if expression_in_statements expr then
          let _ = print_string "Statement" in
          interpret_expressions expr context class_table
          >>= fun new_ctx -> return new_ctx
        else error "Wrong expression in statement"
    | VariableDecl (modifier, variables_type, variable_list) ->
        let is_const = function Some Const -> true | _ -> false in
        let rec helper var_list ctx =
          match var_list with
          | [] -> return ctx
          | (Name var_name, var_expr) :: tail -> (
            match ctx.current_object with
            | NullObjectReference ->
                error "Cannot assign value to variable of null-object"
            | ObjectReference {field_references_table= field_ref_table; _} ->
                ( if
                  Hashtbl.mem ctx.variables_table var_name
                  || Hashtbl.mem field_ref_table var_name
                then error "Variable with this name is already defined"
                else
                  match var_expr with
                  | None ->
                      Hashtbl.add ctx.variables_table var_name
                        { variable_type= variables_type
                        ; variable_key= var_name
                        ; is_const= is_const modifier
                        ; assignments_count= 0
                        ; variable_value= get_type_default_value variables_type
                        ; scope_level= ctx.scope_level } ;
                      return ctx
                  | Some var_ex -> (
                      expressions_type_check var_ex ctx class_table
                      >>= fun var_ex_type ->
                      let add_variable ve =
                        interpret_expressions ve ctx class_table
                        >>= fun ve_ctx ->
                        Hashtbl.add ve_ctx.variables_table var_name
                          { variable_type= var_ex_type
                          ; variable_key= var_name
                          ; is_const= is_const modifier
                          ; assignments_count= 1
                          ; variable_value= ve_ctx.last_expression_result
                          ; scope_level= ve_ctx.scope_level } ;
                        return ve_ctx in
                      match var_ex_type with
                      | TClass "null" -> (
                        match variables_type with
                        | TClass _ -> add_variable var_ex
                        | _ -> error "Wrong assign type in declaration" )
                      | TClass right_class_key -> (
                        match variables_type with
                        | TClass left_class_key ->
                            class_assign_check left_class_key right_class_key
                              class_table
                            >>= fun _ -> add_variable var_ex
                        | _ -> error "Wrong assign type in declaration" )
                      | TArray (TClass right_class_key) -> (
                        match variables_type with
                        | TArray (TClass left_class_key) ->
                            class_assign_check left_class_key right_class_key
                              class_table
                            >>= fun _ -> add_variable var_ex
                        | _ -> error "Wrong assign type in declaration" )
                      | _ when var_ex_type = variables_type ->
                          add_variable var_ex
                      | _ ->
                          error
                            ( "Wrong value type for declared variable: "
                            ^ show_types var_ex_type ) ) )
                >>= fun new_ctx -> helper tail new_ctx ) in
        helper variable_list context

  and interpret_expressions expression context class_table =
    let evaluate_expression expr ctx =
      let evaluate_binary_opeation left right operation =
        interpret_expressions left ctx class_table
        >>= fun left_ctx ->
        interpret_expressions right left_ctx class_table
        >>= fun right_ctx ->
        let left_value = left_ctx.last_expression_result in
        let right_value = right_ctx.last_expression_result in
        let new_value = operation left_value right_value in
        try return {right_ctx with last_expression_result= new_value} with
        | Invalid_argument message -> error message
        | Division_by_zero -> error "Division by zero" in
      let evaluate_unary_opeation operand operation =
        interpret_expressions operand ctx class_table
        >>= fun operand_ctx ->
        let operand_value = operand_ctx.last_expression_result in
        let new_value = operation operand_value in
        try return {operand_ctx with last_expression_result= new_value}
        with Invalid_argument message -> error message in
      match expr with
      | Add (left, right) -> evaluate_binary_opeation left right ( ++ )
      | Sub (left, right) -> evaluate_binary_opeation left right ( -- )
      | Mult (left, right) -> evaluate_binary_opeation left right ( ** )
      | Div (left, right) -> evaluate_binary_opeation left right ( // )
      | Mod (left, right) -> evaluate_binary_opeation left right ( %% )
      | And (left, right) -> evaluate_binary_opeation left right ( &&& )
      | Or (left, right) -> evaluate_binary_opeation left right ( ||| )
      | Not operand -> evaluate_unary_opeation operand ( !!! )
      | Less (left, right) -> evaluate_binary_opeation left right ( <<< )
      | More (left, right) -> evaluate_binary_opeation left right ( >>> )
      | LessOrEqual (left, right) ->
          evaluate_binary_opeation left right ( <<== )
      | MoreOrEqual (left, right) ->
          evaluate_binary_opeation left right ( >>== )
      | Equal (left, right) -> (
          expressions_type_check left ctx class_table
          >>= fun left_type ->
          match left_type with
          | TClass left_key ->
              get_element class_table left_key
              >>= fun left_class ->
              get_element left_class.methods_table equals_key
              >>= fun left_equals ->
              if left_equals.is_overriden then
                interpret_expressions
                  (AccessByPoint
                     (left, CallMethod (Identifier "Equals", [right])))
                  ctx class_table
              else evaluate_binary_opeation left right ( === )
          | _ -> evaluate_binary_opeation left right ( === ) )
      | NotEqual (left, right) -> evaluate_binary_opeation left right ( !=! )
      | Value value ->
          let _ = print_string "Value" in
          return {ctx with last_expression_result= value}
      | Identifier identifier -> (
        match get_element_option ctx.variables_table identifier with
        | Some variable ->
            return {ctx with last_expression_result= variable.variable_value}
        | None -> (
          try
            get_object_info ctx.current_object
            |> fun (_, field_references_table, _) ->
            match get_element_option field_references_table identifier with
            | Some field ->
                return {ctx with last_expression_result= field.field_value}
            | None -> error "No such variable or field"
          with Invalid_argument message | Failure message -> error message ) )
      | Null ->
          return
            { ctx with
              last_expression_result= VObjectReference NullObjectReference }
      | CallMethod (This, arguments) ->
          ( match ctx.current_constructor_key with
          | None ->
              error "this(...) call must be in Constructor(...) : this(...)"
          | Some constructor_key -> return constructor_key )
          >>= fun external_constructor_key ->
          let get_current_class_key =
            match ctx.current_object with
            | NullObjectReference -> error "NullReferenceException"
            | ObjectReference {class_key= key; _} -> return key in
          get_current_class_key
          >>= fun current_class_key ->
          get_element class_table current_class_key
          >>= fun current_class ->
          constructor_check current_class arguments ctx class_table
          >>= fun constructor ->
          if constructor.key = external_constructor_key then
            error "Constructor recursion"
          else
            check_call_constructor constructor.body current_class
              constructor.call_constructor
            >>= fun constructor_body ->
            ( try
                initialize_table_with_arguments (Hashtbl.create 128) arguments
                  constructor.arguments ctx class_table
              with Invalid_argument message -> error message )
            >>= fun (var_table, new_ctx) ->
            interpret_statements constructor_body
              {new_ctx with variables_table= var_table; is_creation= true}
              class_table
            >>= fun new_new_ctx ->
            return
              { new_new_ctx with
                last_expression_result= VVoid
              ; variables_table= ctx.variables_table
              ; constructor_affilation= ctx.constructor_affilation
              ; is_creation= true }
      | CallMethod (Base, arguments) -> (
          ( match ctx.current_constructor_key with
          | None ->
              error "base(...) call must be in Constructor(...) : base(...)"
          | Some constructor_key -> return constructor_key )
          >>= fun _ ->
          ( match ctx.constructor_affilation with
          | None ->
              error "base(...) call must be in Constructor(...) : base(...)"
          | Some constructor_affilation -> return constructor_affilation )
          >>= fun current_class_key ->
          get_element class_table current_class_key
          >>= fun current_class ->
          match current_class.parent_key with
          | None -> error "Bad base(...) call usage: this class has no parent"
          | Some parent_key ->
              get_element class_table parent_key
              >>= fun parent_class ->
              constructor_check parent_class arguments ctx class_table
              >>= fun parent_constructor ->
              check_call_constructor parent_constructor.body parent_class
                parent_constructor.call_constructor
              >>= fun parent_constructor_body ->
              ( try
                  initialize_table_with_arguments (Hashtbl.create 128) arguments
                    parent_constructor.arguments ctx class_table
                with Invalid_argument message -> error message )
              >>= fun (var_table, new_ctx) ->
              interpret_statements parent_constructor_body
                { new_ctx with
                  variables_table= var_table
                ; is_creation= true
                ; constructor_affilation= Some parent_key
                ; current_constructor_key= Some parent_constructor.key }
                class_table
              >>= fun new_new_ctx ->
              return
                { new_new_ctx with
                  last_expression_result= VVoid
                ; variables_table= ctx.variables_table
                ; is_creation= true
                ; constructor_affilation= ctx.constructor_affilation } )
      | This ->
          return
            { ctx with
              last_expression_result= VObjectReference ctx.current_object }
      | AccessByPoint (expr, Identifier field_key) -> (
          interpret_expressions expr ctx class_table
          >>= fun expr_ctx ->
          let obj = expr_ctx.last_expression_result in
          match obj with
          | VObjectReference
              (ObjectReference {field_references_table= field_ref_table; _}) ->
              get_element field_ref_table field_key
              >>= fun field ->
              return {expr_ctx with last_expression_result= field.field_value}
          | _ -> error "Cannot access field of non-object" )
      | AccessByPoint (expr, CallMethod (Identifier method_name, arguments))
        -> (
          interpret_expressions expr ctx class_table
          >>= fun expr_ctx ->
          let obj = expr_ctx.last_expression_result in
          match obj with
          | VObjectReference NullObjectReference ->
              error "NullReferenceException"
          | VObjectReference
              (ObjectReference
                {class_key= key; field_references_table= frt; number= num}) -> (
            match get_element_option class_table key with
            | None -> error "No such class to call method"
            | Some class_t ->
                method_check class_t method_name arguments expr_ctx class_table
                >>= fun method_t ->
                ( match method_t.body with
                | None -> error "Error: abstract class creation"
                | Some body -> return body )
                >>= fun method_body ->
                let new_variables_table : (key_t, variable) Hashtbl_impr.t =
                  Hashtbl.create 128 in
                ( try
                    initialize_table_with_arguments new_variables_table
                      arguments method_t.arguments expr_ctx class_table
                  with Invalid_argument message -> error message )
                >>= fun (new_var_table, new_ctx) ->
                interpret_statements method_body
                  { current_object=
                      ObjectReference
                        { class_key= key
                        ; field_references_table= frt
                        ; number= num }
                  ; variables_table= new_var_table
                  ; last_expression_result= VVoid
                  ; runtime_flag= NoFlag
                  ; current_method_type= method_t.method_type
                  ; is_main_scope= false
                  ; nested_loops_count= 0
                  ; scope_level= 0
                  ; current_constructor_key= None
                  ; previous_context= Some ctx
                  ; objects_created_count= ctx.objects_created_count
                  ; is_creation= false
                  ; constructor_affilation= None }
                  class_table
                >>= fun result_ctx ->
                return
                  { new_ctx with
                    last_expression_result=
                      ( if method_t.method_type = TVoid then VVoid
                      else result_ctx.last_expression_result )
                  ; objects_created_count= result_ctx.objects_created_count
                  ; is_creation= false } )
          | _ -> error "Cannot access field of non-object" )
      | CallMethod (Identifier method_name, arguments) ->
          interpret_expressions
            (AccessByPoint (This, CallMethod (Identifier method_name, arguments)))
            ctx class_table
      | ArrayAccess (arr_expr, index_expr) -> (
          interpret_expressions arr_expr ctx class_table
          >>= fun arr_ctx ->
          interpret_expressions index_expr ctx class_table
          >>= fun ind_ctx ->
          let arr_value = arr_ctx.last_expression_result in
          let ind_value = ind_ctx.last_expression_result in
          match arr_value with
          | VArray (ArrayReference {array_values= array_value_list; _}) -> (
            match ind_value with
            | VInt i when i < 0 || i >= List.length array_value_list ->
                error "IndexOutOfRangeException"
            | VInt i ->
                return
                  { ind_ctx with
                    last_expression_result= List.nth array_value_list i }
            | _ -> error "Index must be int" )
          | VArray NullArrayReference -> error "NullReferenceException"
          | _ -> error "Cannot access a non-array" )
      | ArrayCreationWithSize (arr_type, size_expr) -> (
          interpret_expressions size_expr ctx class_table
          >>= fun size_ctx ->
          let size_value = size_ctx.last_expression_result in
          let def_value = get_type_default_value arr_type in
          match size_value with
          | VInt size ->
              return
                { size_ctx with
                  last_expression_result=
                    VArray
                      (ArrayReference
                         { array_type= arr_type
                         ; array_values= make_list_of_element def_value size
                         ; number= size_ctx.objects_created_count + 1 })
                ; objects_created_count= size_ctx.objects_created_count + 1 }
          | _ -> error "Size must be int" )
      | ArrayCreationWithElements (arr_type, expr_list) ->
          let make_value_list ex_list ex_ctx =
            fold_left
              (fun (arr_list, arr_ctx) ex ->
                interpret_expressions ex arr_ctx class_table
                >>= fun new_ex_ctx ->
                let new_value = new_ex_ctx.last_expression_result in
                return (arr_list @ [new_value], new_ex_ctx))
              ([], ex_ctx) ex_list in
          make_value_list expr_list ctx
          >>= fun (value_list, new_ctx) ->
          return
            { ctx with
              last_expression_result=
                VArray
                  (ArrayReference
                     { array_type= arr_type
                     ; array_values= value_list
                     ; number= new_ctx.objects_created_count + 1 })
            ; objects_created_count= new_ctx.objects_created_count + 1 }
      | ClassCreation (Name class_name, class_arguments) ->
          get_element class_table class_name
          >>= fun object_class ->
          if object_class.is_abstract then
            error "This class is abstract: no object creation allowed"
          else
            constructor_check object_class class_arguments ctx class_table
            >>= fun constructor ->
            let rec initialize_object class_t init_ctx =
              let field_tuples =
                get_variable_field_pairs_list_typed class_t.decl_tree in
              let rec helper_init acc_ht help_ctx = function
                | [] -> return help_ctx
                | (current_field_type, Name current_field_name, field_expression)
                  :: tail ->
                    let is_const_field field_key =
                      get_element object_class.fields_table field_key
                      >>= fun test_field -> return test_field.is_const in
                    ( match field_expression with
                    | Some field_expr -> (
                        expressions_type_check field_expr help_ctx class_table
                        >>= fun expr_type ->
                        is_const_field current_field_name
                        >>= fun const ->
                        let add_field field_ex =
                          interpret_expressions field_ex help_ctx class_table
                          >>= fun field_ex_ctx ->
                          Hashtbl.add acc_ht current_field_name
                            { key= current_field_name
                            ; field_type= current_field_type
                            ; field_value= field_ex_ctx.last_expression_result
                            ; is_const= const
                            ; assignments_count= 1 } ;
                          return (field_ex_ctx, acc_ht) in
                        match expr_type with
                        | TClass "null" -> (
                          match current_field_type with
                          | TClass _ -> add_field field_expr
                          | _ -> error "Wrong assign type in field declaration"
                          )
                        | TClass class_right -> (
                          match current_field_type with
                          | TClass class_left ->
                              class_assign_check class_left class_right
                                class_table
                              >>= fun _ -> add_field field_expr
                          | _ -> error "Wrong assign type in field declaration"
                          )
                        | TArray (TClass class_right) -> (
                          match current_field_type with
                          | TArray (TClass class_left) ->
                              class_assign_check class_left class_right
                                class_table
                              >>= fun _ -> add_field field_expr
                          | _ -> error "Wrong assign type in declaration" )
                        | _ when expr_type = current_field_type ->
                            add_field field_expr
                        | _ -> error "Wrong assign type in declaration" )
                    | None ->
                        is_const_field current_field_name
                        >>= fun const ->
                        Hashtbl.add acc_ht current_field_name
                          { key= current_field_name
                          ; field_type= current_field_type
                          ; field_value=
                              get_type_default_value current_field_type
                          ; is_const= const
                          ; assignments_count= 0 } ;
                        return (help_ctx, acc_ht) )
                    >>= fun (head_ctx, head_ht) ->
                    object_number head_ctx.current_object
                    >>= fun num ->
                    helper_init head_ht
                      { head_ctx with
                        current_object=
                          ObjectReference
                            { class_key= class_name
                            ; field_references_table= head_ht
                            ; number= num } }
                      tail in
              match class_t.parent_key with
              | None -> helper_init (Hashtbl.create 128) init_ctx field_tuples
              | Some parent_key ->
                  get_element class_table parent_key
                  >>= fun parent_class ->
                  initialize_object parent_class init_ctx
                  >>= fun parent_ctx ->
                  helper_init
                    (get_object_fields parent_ctx.current_object)
                    parent_ctx field_tuples in
            let new_object =
              ObjectReference
                { class_key= class_name
                ; field_references_table= Hashtbl.create 128
                ; number= ctx.objects_created_count + 1 } in
            initialize_object object_class
              { current_object= new_object
              ; variables_table= Hashtbl.create 128
              ; last_expression_result= VVoid
              ; runtime_flag= NoFlag
              ; current_method_type= TVoid
              ; is_main_scope= false
              ; nested_loops_count= 0
              ; scope_level= 0
              ; previous_context= Some ctx
              ; objects_created_count= ctx.objects_created_count + 1
              ; current_constructor_key= None
              ; is_creation= false
              ; constructor_affilation= None }
            >>= fun new_ctx ->
            let new_variable_table =
              try
                initialize_table_with_arguments (Hashtbl.create 128)
                  class_arguments constructor.arguments ctx class_table
              with Invalid_argument message -> error message in
            new_variable_table
            >>= fun (var_table, _) ->
            check_call_constructor constructor.body object_class
              constructor.call_constructor
            >>= fun class_body ->
            interpret_statements class_body
              { new_ctx with
                variables_table= var_table
              ; is_creation= true
              ; is_main_scope= false
              ; constructor_affilation= Some object_class.this_key
              ; current_constructor_key= Some constructor.key }
              class_table
            >>= fun class_ctx ->
            return
              { ctx with
                last_expression_result=
                  VObjectReference class_ctx.current_object
              ; runtime_flag= NoFlag
              ; objects_created_count= class_ctx.objects_created_count }
      | Assign (Identifier var_key, val_expr) ->
          let _ = print_string "Assign" in
          interpret_expressions val_expr ctx class_table
          >>= fun val_ctx ->
          update_identifier_value var_key val_ctx.last_expression_result val_ctx
      | Assign (AccessByPoint (obj_expr, Identifier field_name), val_expr) ->
          interpret_expressions val_expr ctx class_table
          >>= fun val_ctx ->
          update_field_value obj_expr field_name val_ctx class_table
      | Assign (ArrayAccess (arr_expr, index_expr), val_expr) -> (
          interpret_expressions val_expr ctx class_table
          >>= fun val_ctx ->
          interpret_expressions arr_expr val_ctx class_table
          >>= fun arr_ctx ->
          interpret_expressions index_expr arr_ctx class_table
          >>= fun index_ctx ->
          match arr_ctx.last_expression_result with
          | VArray arr -> (
            match index_ctx.last_expression_result with
            | VInt i -> (
                let new_val = val_ctx.last_expression_result in
                try
                  update_array_state arr i new_val index_ctx
                  |> fun _ ->
                  return {index_ctx with last_expression_result= new_val}
                with Invalid_argument message | Failure message ->
                  error message )
            | _ -> error "Wrong type for array index" )
          | _ -> error "Wrong type for array asssignment" )
      | PostInc (AccessByPoint (obj_expr, Identifier field))
       |PrefInc (AccessByPoint (obj_expr, Identifier field)) ->
          interpret_expressions
            (Assign
               ( AccessByPoint (obj_expr, Identifier field)
               , Add (AccessByPoint (obj_expr, Identifier field), Value (VInt 1))
               ))
            ctx class_table
      | PostInc (Identifier variable_key) | PrefInc (Identifier variable_key) ->
          interpret_expressions
            (Assign
               ( Identifier variable_key
               , Add (Identifier variable_key, Value (VInt 1)) ))
            ctx class_table
      | PostInc (ArrayAccess (arr_expr, index_expr))
       |PrefInc (ArrayAccess (arr_expr, index_expr)) ->
          interpret_expressions
            (Assign
               ( ArrayAccess (arr_expr, index_expr)
               , Add (ArrayAccess (arr_expr, index_expr), Value (VInt 1)) ))
            ctx class_table
      | PostDec (AccessByPoint (obj_expr, Identifier field))
       |PrefDec (AccessByPoint (obj_expr, Identifier field)) ->
          interpret_expressions
            (Assign
               ( AccessByPoint (obj_expr, Identifier field)
               , Sub (AccessByPoint (obj_expr, Identifier field), Value (VInt 1))
               ))
            ctx class_table
      | PostDec (Identifier variable_key) | PrefDec (Identifier variable_key) ->
          interpret_expressions
            (Assign
               ( Identifier variable_key
               , Sub (Identifier variable_key, Value (VInt 1)) ))
            ctx class_table
      | PostDec (ArrayAccess (arr_expr, index_expr))
       |PrefDec (ArrayAccess (arr_expr, index_expr)) ->
          interpret_expressions
            (Assign
               ( ArrayAccess (arr_expr, index_expr)
               , Sub (ArrayAccess (arr_expr, index_expr), Value (VInt 1)) ))
            ctx class_table
      | _ -> error "Wrong expression construction" in
    expressions_type_check expression context class_table
    >>= fun _ -> interpret_expressions expression context class_table

  and check_call_constructor current_body current_class current_call_constructor
      =
    match current_body with
    | StatementBlock _ -> (
      match (current_call_constructor, current_class.parent_key) with
      | Some (CallMethod (Base, _)), None ->
          error "Base() call in constructor in not child class"
      | Some (CallMethod (Base, _)), Some _ -> return current_body
      | Some (CallMethod (This, _)), _ -> return current_body
      | None, _ -> return current_body
      | _ -> error "Incorrect call constructor in constructor" )
    | _ -> error "Must be statement block in constructor"

  and initialize_table_with_arguments ht argument_list method_argument_list ctx
      class_table =
    fold_left2
      (fun (current_ht, current_ctx) argument type_name_pair ->
        match type_name_pair with
        | current_type, Name current_name ->
            interpret_expressions argument current_ctx class_table
            >>= fun new_ctx ->
            Hashtbl.add current_ht current_name
              { variable_type= current_type
              ; variable_key= current_name
              ; is_const= false
              ; assignments_count= 1
              ; variable_value= new_ctx.last_expression_result
              ; scope_level= 0 } ;
            return (current_ht, new_ctx))
      (ht, ctx) argument_list method_argument_list

  and update_identifier_value variable_key new_value value_ctx =
    if Hashtbl.mem value_ctx.variables_table variable_key then (
      get_element value_ctx.variables_table variable_key
      >>= fun old_variable ->
      variable_check_assign_count old_variable
      >>= fun _ ->
      Hashtbl.replace value_ctx.variables_table variable_key
        { old_variable with
          variable_value= new_value
        ; assignments_count= old_variable.assignments_count + 1 } ;
      return value_ctx )
    else
      match value_ctx.current_object with
      | NullObjectReference -> error "NullReferenceException"
      | ObjectReference {field_references_table= frt; _} ->
          if Hashtbl.mem frt variable_key then
            get_element frt variable_key
            >>= fun old_field ->
            field_check_assign_count old_field
            >>= fun _ ->
            if value_ctx.is_creation then
              Hashtbl.replace frt variable_key
                {old_field with field_value= value_ctx.last_expression_result}
              |> fun _ -> return value_ctx
            else
              try
                update_object_state value_ctx.current_object variable_key
                  value_ctx.last_expression_result value_ctx
                |> fun _ -> return value_ctx
              with
              | Invalid_argument message -> error message
              | Not_found -> error "No such field"
          else error "No such variable"

  and update_field_value obj_expr field_name value_ctx class_table =
    interpret_expressions obj_expr value_ctx class_table
    >>= fun obj_ctx ->
    let obj = get_object_value obj_ctx.last_expression_result in
    let new_value = value_ctx.last_expression_result in
    try
      get_object_info obj
      |> fun (_, frt, _) ->
      if Hashtbl.mem frt field_name then
        get_element frt field_name
        >>= fun old_field ->
        field_check_assign_count old_field
        >>= fun _ ->
        if obj_ctx.is_creation then
          Hashtbl.replace frt field_name
            {old_field with field_value= value_ctx.last_expression_result}
          |> fun _ -> return obj_ctx
        else
          update_object_state obj field_name new_value obj_ctx
          |> fun _ -> return obj_ctx
      else error "No such field in class"
    with
    | Invalid_argument message | Failure message -> error message
    | Not_found -> error "No such field"

  and update_object_state obj field_key new_value update_ctx =
    let rec update_states ht field_key new_value obj_number assign_count =
      Hashtbl.iter
        (fun _ field_reference ->
          match field_reference with
          | {field_value= field_val; _} -> (
            match field_val with
            | VObjectReference
                (ObjectReference {field_references_table= frt; number= num; _})
              ->
                ( if num = obj_number then
                  match get_element_option frt field_key with
                  | None -> raise Not_found
                  | Some old_field ->
                      Hashtbl.replace frt field_key
                        { old_field with
                          field_value= new_value
                        ; assignments_count= assign_count } ) ;
                update_states frt field_key new_value obj_number assign_count
            | VArray
                (ArrayReference
                  {array_type= TClass _; array_values= value_list; _}) ->
                List.iter
                  (fun value ->
                    match value with
                    | VObjectReference
                        (ObjectReference
                          {field_references_table= frt; number= num; _}) ->
                        ( if num = obj_number then
                          match get_element_option frt field_key with
                          | None -> raise Not_found
                          | Some old_field ->
                              Hashtbl.replace frt field_key
                                { old_field with
                                  field_value= new_value
                                ; assignments_count= assign_count } ) ;
                        update_states frt field_key new_value obj_number
                          assign_count
                    | _ -> ())
                  value_list
            | _ -> () ))
        ht in
    let rec helper_update field_key new_value ctx obj_num assign_count =
      Hashtbl.iter
        (fun _ variable ->
          match variable.variable_value with
          | VObjectReference
              (ObjectReference {field_references_table= frt; number= num; _}) ->
              if obj_num = num then (
                match get_element_option frt field_key with
                | None -> raise Not_found
                | Some old_field ->
                    Hashtbl.replace frt field_key
                      { old_field with
                        field_value= new_value
                      ; assignments_count= assign_count } ;
                    update_states frt field_key new_value obj_num assign_count )
              else update_states frt field_key new_value obj_num assign_count
          | VArray
              (ArrayReference
                {array_type= TClass _; array_values= value_list; _}) ->
              List.iter
                (fun value ->
                  match value with
                  | VObjectReference
                      (ObjectReference
                        {field_references_table= frt; number= num; _}) ->
                      ( if num = obj_num then
                        match get_element_option frt field_key with
                        | None -> raise Not_found
                        | Some old_field ->
                            Hashtbl.replace frt field_key
                              { old_field with
                                field_value= new_value
                              ; assignments_count= assign_count } )
                      |> fun () ->
                      update_states frt field_key new_value obj_num assign_count
                  | _ -> ())
                value_list
          | _ -> ())
        ctx.variables_table
      |> fun () ->
      match ctx.previous_context with
      | None -> ()
      | Some previous_ctx ->
          helper_update field_key new_value previous_ctx obj_num assign_count
    in
    get_object_info obj
    |> fun (_, object_frt, object_number) ->
    ( match get_element_option object_frt field_key with
    | None -> raise Not_found
    | Some field -> field.assignments_count + 1 )
    |> fun assignments_count ->
    helper_update field_key new_value update_ctx object_number assignments_count

  and update_array_state array index new_value update_ctx =
    let rec update_states ht i new_value array_number =
      Hashtbl.iter
        (fun _ field_reference ->
          match field_reference with
          | {key= field_key; field_value= field_val; _} -> (
            match field_val with
            | VArray
                (ArrayReference
                  { array_type= arr_type
                  ; array_values= current_values
                  ; number= current_number }) -> (
                if current_number = array_number then
                  Hashtbl.replace ht field_key
                    { field_reference with
                      field_value=
                        update_array_value
                          (VArray
                             (ArrayReference
                                { array_type= arr_type
                                ; array_values= current_values
                                ; number= current_number }))
                          i new_value } ;
                match arr_type with
                | TClass _ ->
                    List.iter
                      (fun value ->
                        match value with
                        | VObjectReference
                            (ObjectReference {field_references_table= frt; _})
                          ->
                            update_states frt i new_value array_number
                        | _ -> ())
                      current_values
                | _ -> () )
            | VObjectReference
                (ObjectReference {field_references_table= frt; _}) ->
                update_states frt i new_value array_number
            | _ -> () ))
        ht in
    let rec helper_update i new_value ctx array_number =
      Hashtbl.iter
        (fun variable_key variable ->
          match variable.variable_value with
          | VObjectReference (ObjectReference {field_references_table= frt; _})
            ->
              update_states frt i new_value array_number
          | VArray
              (ArrayReference
                { array_type= arr_type
                ; array_values= current_values
                ; number= current_number }) -> (
              if current_number = array_number then
                Hashtbl.replace ctx.variables_table variable_key
                  { variable with
                    variable_value=
                      update_array_value
                        (VArray
                           (ArrayReference
                              { array_type= arr_type
                              ; array_values= current_values
                              ; number= current_number }))
                        i new_value }
                |> fun () ->
                match arr_type with
                | TClass _ ->
                    List.iter
                      (fun value ->
                        match value with
                        | VObjectReference
                            (ObjectReference {field_references_table= frt; _})
                          ->
                            update_states frt i new_value array_number
                        | _ -> ())
                      current_values
                | _ -> () )
          | _ -> ())
        ctx.variables_table
      |> fun () ->
      match ctx.previous_context with
      | None -> ()
      | Some previous_ctx -> helper_update i new_value previous_ctx array_number
    in
    get_array_info array
    |> fun (_, _, arr_number) ->
    helper_update index new_value update_ctx arr_number

  let execute ht =
    find_class_with_main ht
    >>= fun class_t ->
    context_initialize
      (ObjectReference
         { class_key= class_t.this_key
         ; field_references_table= Hashtbl.create 16
         ; number= 0 })
      (Hashtbl.create 16)
    >>= fun ctx ->
    let main = Hashtbl.find class_t.methods_table "Main" in
    match main.body with
    | None -> error "Main() method cannot be abstract"
    | Some main_body -> interpret_statements main_body ctx ht
end
