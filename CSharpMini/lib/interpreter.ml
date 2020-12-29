open Hashtbl
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

type constructor_r =
  { arguments: (types * names) list
  ; call_constructor: expressions option
  ; body: statements }
[@@deriving show {with_path= false}]

type field_r =
  {field_type: types; key: key_t; is_const: bool; sub_tree: expressions option}
[@@deriving show {with_path= false}]

type method_r =
  { method_type: types
  ; is_abstract: bool
  ; is_virtual: bool
  ; is_override: bool
  ; arguments: (types * names) list
  ; key: key_t
  ; body: statements option }
[@@deriving show {with_path= false}]

type class_r =
  { this_key: key_t
  ; parent_key: key_t option
  ; children_keys: key_t list
  ; is_abstract: bool
  ; fields_table: (key_t, field_r) Hashtbl.t
  ; methods_table: (key_t, method_r) Hashtbl.t
  ; constructors_table: (key_t, constructor_r) Hashtbl.t
  ; decl_tree: classes }

let class_table : (key_t, class_r) Hashtbl.t = Hashtbl.create 1024
let convert_name_to_key = function Some (Name x) -> Some x | None -> None

let convert_table_to_list ht =
  Hashtbl.fold (fun _ value tl -> value :: tl) ht []

let convert_table_to_sequence = Hashtbl.to_seq_values
let get_element_option = Hashtbl.find_opt

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

  let replace_element_moderr ht old_key new_value =
    Hashtbl.replace ht old_key new_value ;
    return ht

  let rec list_iter_moderr list action base =
    match list with
    | [] -> return base
    | x :: xs -> action x >> list_iter_moderr xs action base

  let prepare_object ht =
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

  let check_modifiers_field_method_constructor pair =
    match pair with
    | modifier_list, field_method_constructor -> (
      match field_method_constructor with
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
      | Method (TVoid, Name "main", [], _)
        when is_static modifier_list
             && (not (is_abstract modifier_list))
             && (not (is_virtual modifier_list))
             && not (is_override modifier_list) ->
          return ()
      | Method (_, Name "main", _, _) ->
          error "Only one main method can be in the program"
      | Method (_, _, _, _) when is_static modifier_list ->
          error "Static methods other than main are not supported"
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

  let add_with_check ht key value error_message =
    match get_element_option ht key with
    | None -> Hashtbl.add ht key value ; return ht
    | Some _ -> error error_message

  let add_to_class_table ht class_to_add =
    match class_to_add with
    | Class
        (modifier_list_class, Name this_key, class_parent, class_elements_list)
      ->
        let methods_table = Hashtbl.create 1024 in
        let fields_table = Hashtbl.create 1024 in
        let construtors_table = Hashtbl.create 1024 in
        check_modifiers_class class_to_add
        >>
        let add_element element =
          match element with
          | field_modifier_list, Field (field_type, field_pairs) ->
              let rec adder = function
                | [] -> return ()
                | (Name key, sub_tree) :: tail ->
                    let is_const = is_const field_modifier_list in
                    let field = {field_type; key; is_const; sub_tree} in
                    add_with_check fields_table key field "Similar fields"
                    >> adder tail in
              check_modifiers_field_method_constructor element
              >> adder field_pairs
          | ( method_modifier_list
            , Method (method_type, Name method_name, arguments, body) ) ->
              let key =
                String.concat ""
                  (method_name :: List.map show_types (get_type_list arguments))
              in
              let is_abstract_class = is_abstract modifier_list_class in
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
                  | None -> error "Body missing in non-abstract method" ) in
              let is_virtual = is_virtual method_modifier_list in
              let is_override = is_override method_modifier_list in
              let methodq =
                { method_type
                ; is_abstract
                ; is_virtual
                ; is_override
                ; arguments
                ; key
                ; body } in
              check_modifiers_field_method_constructor element
              >> is_abstract_body
              >> add_with_check methods_table key methodq
                   "Method with this type exists"
              >> return ()
          | _ -> return () in
        (*TODO: Constr*)
        return ()

  (*TODO: Class*)
end
