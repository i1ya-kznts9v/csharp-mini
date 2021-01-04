open Ast
open Parser

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val get : 'a t -> 'a
  val error : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let ( >> ) x f = x >>= fun _ -> f
  let return = Result.ok
  let error = Result.error
  let get = Result.get_ok
end

type key_t = string [@@deriving show {with_path= false}]

type constructor_t =
  { arguments: (types * names) list
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
  ; body: statements option }
[@@deriving show {with_path= false}]

type class_t =
  { this_key: key_t
  ; fields_table: (key_t, field_t) Hashtbl.t
  ; methods_table: (key_t, method_t) Hashtbl.t
  ; constructors_table: (key_t, constructor_t) Hashtbl.t
  ; children_keys: key_t list
  ; is_abstract: bool
  ; is_sealed: bool
  ; parent_key: key_t option
  ; decl_tree: classes }

let class_table : (key_t, class_t) Hashtbl.t = Hashtbl.create 1024
let convert_name_to_key = function Some (Name x) -> Some x | None -> None

let convert_table_to_list ht =
  Hashtbl.fold (fun _ value tl -> value :: tl) ht []

let get_element_option = Hashtbl.find_opt

let starts_with test_str sub_str =
  let sub_test_str = String.sub test_str 0 (String.length sub_str) in
  String.equal sub_test_str sub_str

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

  let update_element_monerr ht old_key new_value =
    Hashtbl.replace ht old_key new_value ;
    return ht

  let rec list_map_monerr list action base =
    match list with
    | [] -> return base
    | x :: xs -> action x >> list_map_monerr xs action base

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
      ; key= "equals"
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
      ; key= "toString"
      ; body=
          apply Statement.statement_block
            {|
        {
          return "Object";
        }
      |} } in
    let object_hardcode =
      { this_key= "Object"
      ; fields_table
      ; methods_table
      ; constructors_table
      ; children_keys= []
      ; is_abstract= false
      ; is_sealed= false
      ; parent_key= None
      ; decl_tree=
          Option.get
            (apply class_decl
               {|
                public class Object 
                {
                    public int equals(Object obj) 
                    {
                      if (this == obj) return 1;
                      else return 0;
                    }
                          
                    public String toString() 
                    {
                      return "Object";
                    }
                  }
|})
      } in
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

  let get_type_list = List.map fst

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
                let key =
                  String.concat ""
                    (name :: List.map show_types (get_type_list arguments))
                in
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
                  ; body } in
                check_modifiers_element element
                >> virtual_in_body >> is_abstract_body
                >> add_with_check methods_table key method_t
                     "Method with this type exists"
                >> return ()
            | _, Constructor (Name name, arguments, call_constructor, body) ->
                let key =
                  String.concat ""
                    (name :: List.map show_types (get_type_list arguments))
                in
                let match_name_with_class =
                  if name = this_key then return ()
                  else error "Constructor name does not match class name" in
                let constructor_t = {arguments; call_constructor; body} in
                match_name_with_class
                >> check_modifiers_element element
                >> add_with_check constructors_table key constructor_t
                     "Constructor with this type exists"
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
          list_map_monerr class_element_list add_element ()
          >> add_with_check ht this_key class_t "This class already exists"
    in
    list_map_monerr class_list (add_to_class_table ht) ht

  let many_add_default_constructor ht =
    let constructor_t =
      {arguments= []; call_constructor= None; body= StatementBlock []} in
    Hashtbl.iter
      (fun key class_t ->
        if Hashtbl.length class_t.constructors_table = 0 then
          Hashtbl.add class_t.constructors_table key constructor_t)
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
              update_element_monerr ht parent_key new_parent
              >> return new_parent
          | Some _ -> error "Sealed or static class cannot be inherited" ) in
    list_map_monerr (convert_table_to_list ht) update ht

  (*Functions for implementing inheritance*)

  let many_field_inheritance parent children =
    let parent_field_inheritance : class_t -> field_t -> unit t =
     fun children parent_field ->
      match get_element_option children.fields_table parent_field.key with
      | None ->
          return
            (Hashtbl.add children.fields_table parent_field.key parent_field)
      | Some _ -> return () in
    list_map_monerr
      (convert_table_to_list parent.fields_table)
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
    list_map_monerr
      (convert_table_to_list parent.constructors_table)
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
    list_map_monerr
      (convert_table_to_list children.constructors_table)
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
      | _ -> return () in
    list_map_monerr
      (convert_table_to_list parent.methods_table)
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
    list_map_monerr
      (convert_table_to_list children.methods_table)
      (check_children_method_override parent)
      ()

  let rec transfert : (key_t, class_t) Hashtbl.t -> class_t -> class_t -> unit t
      =
   fun ht parent children ->
    many_field_inheritance parent children
    >> many_method_inheritance parent children
    >> many_check_method_override parent children
    >> many_check_parent_constructor parent
    >> many_check_children_constructor children
    >>= fun _ -> transfert_on_children ht children

  and transfert_on_children ht children =
    let childrens_of_children = children.children_keys in
    list_map_monerr childrens_of_children
      (fun children_children_key ->
        transfert ht children
          (Option.get (get_element_option ht children_children_key)))
      ()

  let inheritance ht =
    let object_t = Option.get (get_element_option ht "Object") in
    let processing children_key =
      transfert ht object_t (Option.get (get_element_option ht children_key))
    in
    list_map_monerr object_t.children_keys processing ht

  let load class_list =
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

  type context =
    { current_object: object_references
    ; variables_table: (key_t, variable) Hashtbl_impr.t
    ; last_expression_result: values option
    ; was_break: bool
    ; was_continue: bool
    ; was_return: bool
    ; current_method_type: types
    ; is_main: bool
    ; cycles_count: int
    ; scope_level: int
    ; is_constructor: bool
    ; main_context: context option
    ; objects_created_count: int
    ; is_creation: bool }
  [@@deriving show {with_path= false}]

  let context_initialize current_object variables_table =
    return
      { current_object
      ; variables_table
      ; last_expression_result= None
      ; was_break= false
      ; was_continue= false
      ; was_return= false
      ; current_method_type= TVoid
      ; is_main= true
      ; cycles_count= 0
      ; scope_level= 0
      ; is_constructor= false
      ; main_context= None
      ; objects_created_count= 0
      ; is_creation= false }

  let get_main_context current_context =
    match current_context.main_context with
    | None -> current_context
    | Some main_context -> main_context

  let get_object_number = function
    | NullObjectReference -> error "NullReferenceException"
    | ObjectReference {class_key= _; field_references_table= _; number= num} ->
        return num

  let find_class_with_main ht =
    return
      (List.find
         (fun class_t -> Hashtbl.mem class_t.methods_table "Main")
         (convert_table_to_list ht))

  let rec expressions_type_check : expressions -> context -> types t =
   fun expression context ->
    match expression with
    | Add (left, right) -> (
        expressions_type_check left context
        >>= fun left_type ->
        match left_type with
        | TInt -> (
            expressions_type_check right context
            >>= fun right_type ->
            match right_type with
            | TInt -> return TInt
            | TString -> return TString
            | _ -> error "Wrong type: must be <int> or <string>" )
        | TString -> (
            expressions_type_check right context
            >>= fun right_type ->
            match right_type with
            | TInt | TString -> return TString
            | _ -> error "Wrong type: must be <int> or <string>" )
        | _ -> error "Wrong type: must be <int> or <string>" )
    | Sub (left, right)
     |Div (left, right)
     |Mod (left, right)
     |Mult (left, right) -> (
        expressions_type_check left context
        >>= fun left_type ->
        match left_type with
        | TInt -> (
            expressions_type_check right context
            >>= fun right_type ->
            match right_type with
            | TInt -> return TInt
            | _ -> error "Wrong type: must be <int>" )
        | _ -> error "Wrong type: must be <int>" )
    | PostDec value | PostInc value | PrefDec value | PrefInc value -> (
        expressions_type_check value context
        >>= fun value_type ->
        match value_type with
        | TInt -> return TInt
        | _ -> error "Wrong type: must be <int>" )
    | And (left, right) | Or (left, right) -> (
        expressions_type_check left context
        >>= fun left_type ->
        match left_type with
        | TBool -> (
            expressions_type_check right context
            >>= fun right_type ->
            match right_type with
            | TBool -> return TBool
            | _ -> error "Wrong type: must be <bool>" )
        | _ -> error "Wrong type: must be <bool>" )
    | Not value -> (
        expressions_type_check value context
        >>= fun value_type ->
        match value_type with
        | TBool -> return TBool
        | _ -> error "Wrong type: must be <bool>" )
    | Less (left, right)
     |More (left, right)
     |LessOrEqual (left, right)
     |MoreOrEqual (left, right) -> (
        expressions_type_check left context
        >>= fun left_type ->
        match left_type with
        | TInt -> (
            expressions_type_check right context
            >>= fun right_type ->
            match right_type with
            | TInt -> return TBool
            | _ -> error "Wrong type: must be <int>" )
        | _ -> error "Wrong type: must be <int>" )
    | Equal (left, right) | NotEqual (left, right) -> (
        expressions_type_check left context
        >>= fun left_type ->
        match left_type with
        | TInt -> (
            expressions_type_check right context
            >>= fun right_type ->
            match right_type with
            | TInt -> return TBool
            | _ -> error "Wrong type: must be <int>" )
        | TString -> (
            expressions_type_check right context
            >>= fun right_type ->
            match right_type with
            | TString -> return TBool
            | _ -> error "Wrong type: must be <string>" )
        | TBool -> (
            expressions_type_check right context
            >>= fun right_type ->
            match right_type with
            | TBool -> return TBool
            | _ -> error "Wrong type: must be <bool>" )
        | TArray left_array_type -> (
            expressions_type_check right context
            >>= fun right_type ->
            match right_type with
            | TArray right_array_type when left_array_type = right_array_type ->
                return TBool
            | _ -> error "Wrong type: must be <same_types[]>" )
        | TClass left_class_name -> (
            expressions_type_check right context
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
          let parent_key =
            Option.get
              (Option.get (get_element_option class_table key)).parent_key in
          return (TClass parent_key)
      | NullObjectReference -> error "Current object is null in context" )
    | CallMethod (Base, _) -> return TVoid
    | CallMethod (This, _) -> return TVoid
    | CallMethod (Identifier method_name, arguments) ->
        let current_object_key =
          match context.current_object with
          | NullObjectReference -> "null"
          | ObjectReference {class_key= key; _} -> key in
        let current_class =
          Option.get (get_element_option class_table current_object_key) in
        method_check current_class method_name arguments context
        >>= fun method_t -> return method_t.method_type
    | AccessByPoint (object_expression, Identifier field_key) -> (
        expressions_type_check object_expression context
        >>= fun object_type ->
        match object_type with
        | TClass "null" -> error "NullReferenceException"
        | TClass class_key -> (
            let class_t =
              Option.get (get_element_option class_table class_key) in
            let field_t = get_element_option class_t.fields_table field_key in
            match field_t with
            | None -> error "No such field in class"
            | Some field_t -> return field_t.field_type )
        | _ -> error "Wrong type: must be an object of some class" )
    | AccessByPoint
        (object_expression, CallMethod (Identifier method_name, arguments)) -> (
        expressions_type_check object_expression context
        >>= fun object_type ->
        match object_type with
        | TClass "null" -> error "NullReferenceException"
        | TClass class_key ->
            let class_t =
              Option.get (get_element_option class_table class_key) in
            method_check class_t method_name arguments context
            >>= fun method_t -> return method_t.method_type
        | _ -> error "Wrong type: must be an object of some class" )
    | ArrayAccess (array_expression, index_expression) -> (
        expressions_type_check index_expression context
        >>= fun index_type ->
        match index_type with
        | TInt -> (
            expressions_type_check array_expression context
            >>= fun array_type ->
            match array_type with
            | TArray array_type -> return array_type
            | _ -> error "Wrong type: must be <some_type[]>" )
        | _ -> error "Wrong type: index must be <int>" )
    | ArrayCreationWithSize (array_type, size) -> (
        expressions_type_check size context
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
              expressions_type_check element context
              >>= fun element_type ->
              match array_type with
              | TClass array_class_name -> (
                match element_type with
                | TClass "null" -> elements_check tail
                | TClass element_class_name ->
                    class_assign_check array_class_name element_class_name
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
        | None -> error "No such class implemented"
        | Some class_t -> (
          match arguments with
          | [] -> return (TClass class_name)
          | _ ->
              constructor_check class_t arguments context
              >> return (TClass class_name) ) )
    | Identifier key -> (
        let variable = get_element_option context.variables_table key in
        match variable with
        | None -> (
          match context.current_object with
          | ObjectReference
              {class_key= _; field_references_table= field_table; number= _}
            -> (
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
        expressions_type_check left context
        >>= fun left_type ->
        match left_type with
        | TVoid -> error "Can't assign anything to <void>"
        | TClass left_class_key -> (
            expressions_type_check right context
            >>= fun right_type ->
            match right_type with
            | TClass "null" -> return (TClass left_class_key)
            | TClass right_class_key ->
                class_assign_check left_class_key right_class_key
            | _ -> error "Wrong assign types" )
        | TArray (TClass left_class_key) -> (
            expressions_type_check right context
            >>= fun right_type ->
            match right_type with
            | TArray (TClass right_class_key) ->
                class_assign_check left_class_key right_class_key
            | _ -> error "Wrong assign types" )
        | _ ->
            expressions_type_check right context
            >>= fun right_type ->
            if left_type = right_type then return right_type
            else error "Wrong assign types" )
    | _ -> error "Wrong expression"

  and class_type_polymorphism_check left_class_name right_class_name =
    let rec check_parents class_key =
      let class_t = Option.get (get_element_option class_table class_key) in
      if class_t.this_key = left_class_name then true
      else
        match class_t.parent_key with
        | None -> false
        | Some parent_key -> check_parents parent_key in
    check_parents right_class_name

  and class_assign_check left_class_key right_class_key =
    match class_type_polymorphism_check left_class_key right_class_key with
    | false -> error "Cannot assign the most general type to the least general"
    | true -> return (TClass right_class_key)

  and constructor_check class_t arguments context =
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
          | _ -> false )
        | _ -> found_type = argument_type ) in
    let rec helper ht position arguments context =
      match Hashtbl.length ht with
      | 0 -> error "No such constructor implemented"
      | other -> (
        match arguments with
        | [] -> (
          match other with
          | 1 -> return (List.hd (convert_table_to_list ht))
          | _ -> error "Cannot resolve constructor" )
        | argument :: tail ->
            expressions_type_check argument context
            >>= fun argument_type ->
            helper
              (Hashtbl_impr.filter ht (type_check position argument_type))
              (position + 1) tail context ) in
    helper
      (Hashtbl_impr.filter class_t.constructors_table (fun _ constructor_t ->
           List.length constructor_t.arguments = List.length arguments))
      0 arguments context

  (* and method_check class_t method_name arguments context =
     let type_check : int -> types -> key_t -> method_t -> bool =
     fun position argument_type _ method_t ->
       match List.nth_opt method_t.arguments position with
       | None -> false
       | Some (found_type, _) ->
         (match argument_type with
         | TClass "null" ->
           (match found_type with
           | TClass _ -> true
           | _ -> false)
         | TClass class_key ->
           (match found_type with
           | TClass found_class_key -> class_type_polymorphism_check found_class_key class_key
           | _ -> false)
         | _ -> found_type = argument_type)
       in
       let rec helper ht position arguments context =
         match Hashtbl.length ht with
         | 0 -> error "No such method implemented"
         | other ->
           ( match arguments with
           | [] ->
             (match other with
             | 1 -> return (List.hd (convert_table_to_list ht))
             | _ ->
               (Hashtbl_impr.filter ht (fun _ method_t -> starts_with method_t.key method_name)
               |> fun ht_filtred_by_name -> match Hashtbl.length ht_filtred_by_name with
               | 1 -> return (List.hd (convert_table_to_list ht_filtred_by_name))
               | _ -> error "Cannot resolve method")
           | argument :: tail ->
             (expressions_type_check argument context >>= fun argument_type ->
             helper (Hashtbl_impr.filter ht (type_check position argument_type)) (position + 1) tail context))
         in
         helper (Hashtbl_impr.filter class_t.methods_table (fun _ method_t -> List.length method_t.arguments = List.length arguments)) 0 arguments context *)
  and method_check class_t method_name arguments context =
    Hashtbl_impr.filter class_t.methods_table (fun _ method_t ->
        starts_with method_t.key method_name)
    |> fun methods_table_filtred_by_name ->
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
          | _ -> false )
        | _ -> found_type = argument_type ) in
    let rec helper ht_filtred_by_argument position arguments context =
      match Hashtbl.length ht_filtred_by_argument with
      | 0 -> error "No such method implemented"
      | other -> (
        match arguments with
        | [] -> (
          match other with
          | 1 -> return (List.hd (convert_table_to_list ht_filtred_by_argument))
          | _ -> error "Cannot resolve method" )
        | argument :: tail ->
            expressions_type_check argument context
            >>= fun argument_type ->
            helper
              (Hashtbl_impr.filter ht_filtred_by_argument
                 (type_check position argument_type))
              (position + 1) tail context ) in
    helper
      (Hashtbl_impr.filter methods_table_filtred_by_name (fun _ method_t ->
           List.length method_t.arguments = List.length arguments))
      0 arguments context

  let get_int_value = function VInt value -> value | _ -> 0
  let get_string_value = function VString value -> value | _ -> ""
  let get_bool_value = function VBool value -> value | _ -> false

  let get_object_value = function
    | VObjectReference value -> value
    | _ -> NullObjectReference

  let get_type_default_value = function
    | TInt -> VInt 0
    | TString -> VString ""
    | TClass _ -> VObjectReference NullObjectReference
    | TBool -> VBool false
    | TVoid -> VVoid
    | TArray _ -> VArray NullArrayReference

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

  let rec interpret_statements : statements -> context -> context t =
   fun statement context ->
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
            | _ when ctx.cycles_count >= 1 && ctx.was_break -> return ctx
            | _ when ctx.cycles_count >= 1 && ctx.was_continue -> return ctx
            | _ when ctx.was_return -> return ctx
            | _ ->
                interpret_statements stat ctx
                >>= fun new_ctx -> helper tail new_ctx ) in
        helper statement_list context
        >>= fun ctx ->
        if ctx.is_main then return ctx else delete_variable_from_scope ctx
    | While (expr, stat) -> (
        let was_main = context.is_main in
        let rec loop st ctx =
          if ctx.was_break then
            match st with
            | StatementBlock _ ->
                return
                  (decrement_scope_level
                     { ctx with
                       was_break= false
                     ; cycles_count= ctx.cycles_count - 1 })
            | _ ->
                return
                  {ctx with was_break= false; cycles_count= ctx.cycles_count - 1}
          else
            interpret_expressions expr ctx
            >>= fun new_ctx ->
            match new_ctx.last_expression_result with
            | Some (VBool false) -> (
              match st with
              | StatementBlock _ ->
                  return
                    (decrement_scope_level
                       { new_ctx with
                         cycles_count= ctx.cycles_count - 1
                       ; is_main= was_main })
              | _ -> return {new_ctx with cycles_count= ctx.cycles_count - 1} )
            | Some (VBool true) ->
                interpret_statements st new_ctx
                >>= fun new_new_ctx ->
                if new_new_ctx.was_return then return new_new_ctx
                else if new_new_ctx.was_continue then
                  loop st {new_new_ctx with was_continue= false}
                else loop st new_new_ctx
            | _ -> error "Wrong expression type for loop <while> condition"
        in
        match stat with
        | StatementBlock _ ->
            loop stat
              (increment_scope_level
                 { context with
                   cycles_count= context.cycles_count + 1
                 ; is_main= false })
        | _ -> loop stat {context with cycles_count= context.cycles_count + 1} )
    | Break ->
        if context.cycles_count <= 0 then error "No loop for <break>"
        else return {context with was_break= true}
    | Continue ->
        if context.cycles_count <= 0 then error "No loop for <continue>"
        else return {context with was_continue= true}
    | If (expr, stat_body, stat_else) -> (
        interpret_expressions expr context
        >>= fun new_ctx ->
        let was_main = new_ctx.is_main in
        match new_ctx.last_expression_result with
        | Some (VBool true) -> (
          match stat_body with
          | StatementBlock _ ->
              interpret_statements stat_body
                (increment_scope_level {new_ctx with is_main= false})
              >>= fun new_new_ctx ->
              return
                (decrement_scope_level {new_new_ctx with is_main= was_main})
          | _ -> interpret_statements stat_body new_ctx )
        | Some (VBool false) -> (
          match stat_else with
          | Some st_else -> (
            match st_else with
            | StatementBlock _ ->
                interpret_statements st_else
                  (increment_scope_level {new_ctx with is_main= false})
                >>= fun new_new_ctx ->
                return
                  (decrement_scope_level {new_new_ctx with is_main= was_main})
            | _ -> interpret_statements st_else new_ctx )
          | None -> return context )
        | _ -> error "Wrong expression type in <if> condition" )
    | For (decl_stat, cond_expr, after_expr, body_stat) ->
        let was_main = context.is_main in
        ( match decl_stat with
        | None -> return (increment_scope_level {context with is_main= false})
        | Some decl_st ->
            interpret_statements decl_st
              (increment_scope_level {context with is_main= false}) )
        >>= fun decl_ctx ->
        let rec loop body_st after_ex ctx =
          if ctx.was_break then
            delete_variable_from_scope
              { ctx with
                was_break= false
              ; cycles_count= ctx.cycles_count - 1
              ; scope_level= ctx.scope_level - 1
              ; is_main= was_main }
          else
            ( match cond_expr with
            | None -> return {ctx with last_expression_result= Some (VBool true)}
            | Some cond_ex -> interpret_expressions cond_ex ctx )
            >>= fun cond_ctx ->
            match cond_ctx.last_expression_result with
            | Some (VBool false) ->
                delete_variable_from_scope
                  { cond_ctx with
                    cycles_count= cond_ctx.cycles_count - 1
                  ; scope_level= cond_ctx.scope_level - 1
                  ; is_main= was_main }
            | Some (VBool true) ->
                let rec interpret_after_ex expr_list cont =
                  match expr_list with
                  | [] -> return cont
                  | expr :: tail -> (
                    match expr with
                    | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
                     |Assign (_, _)
                     |CallMethod (_, _)
                     |AccessByPoint (_, CallMethod (_, _)) ->
                        interpret_expressions expr cont
                        >>= fun new_ctx -> interpret_after_ex tail new_ctx
                    | _ -> error "Wrong expression in after body" ) in
                interpret_statements body_st
                  {cond_ctx with scope_level= decl_ctx.scope_level + 1}
                >>= fun body_ctx ->
                if body_ctx.was_return then
                  return {body_ctx with is_main= was_main}
                else if body_ctx.was_continue then
                  loop body_st after_ex {body_ctx with was_continue= false}
                else
                  interpret_after_ex after_ex body_ctx
                  >>= fun after_ctx -> loop body_st after_ex after_ctx
            | _ -> error "Wrong expression type in loop <for> condition" in
        loop body_stat after_expr decl_ctx
    | Return expr -> (
      match expr with
      | None when context.current_method_type = TVoid ->
          return
            {context with last_expression_result= Some VVoid; was_return= true}
      | None -> error "Return value type mismatch"
      | Some ex ->
          expressions_type_check ex context
          >>= fun ex_type ->
          if ex_type <> context.current_method_type then
            error "Return value type mismatch"
          else
            interpret_expressions ex context
            >>= fun new_ctx -> return {new_ctx with was_return= true} )
    | Expression expr -> (
      match expr with
      | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
       |CallMethod (_, _)
       |AccessByPoint (_, CallMethod (_, _))
       |Assign (_, _) ->
          interpret_expressions expr context >>= fun new_ctx -> return new_ctx
      | _ -> error "Wrong expression in statement" )
    | VariableDecl (modifier, variables_type, variable_list) ->
        let is_const = function Some Const -> true | _ -> false in
        let rec helper var_list ctx =
          match var_list with
          | [] -> return ctx
          | (Name var_name, var_expr) :: tail -> (
            match ctx.current_object with
            | NullObjectReference ->
                error "Cannot assign value to variable of null-object"
            | ObjectReference
                { class_key= _
                ; field_references_table= field_ref_table
                ; number= _ } ->
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
                      expressions_type_check var_ex ctx
                      >>= fun var_ex_type ->
                      let add_variable ve =
                        interpret_expressions ve ctx
                        >>= fun ve_ctx ->
                        Hashtbl.add ve_ctx.variables_table var_name
                          { variable_type= var_ex_type
                          ; variable_key= var_name
                          ; is_const= is_const modifier
                          ; assignments_count= 1
                          ; variable_value=
                              Option.get ve_ctx.last_expression_result
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
                            >>= fun _ -> add_variable var_ex
                        | _ -> error "Wrong assign type in declaration" )
                      | TArray (TClass right_class_key) -> (
                        match variables_type with
                        | TArray (TClass left_class_key) ->
                            class_assign_check left_class_key right_class_key
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
end
