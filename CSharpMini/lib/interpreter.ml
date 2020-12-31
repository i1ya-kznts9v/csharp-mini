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
      ; is_sealed= true
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

  let many_add_to_class_table class_list ht =
    let add_with_check ht key value error_message =
      match get_element_option ht key with
      | None -> Hashtbl.add ht key value ; return ht
      | Some _ -> error error_message in
    let add_to_class_table ht class_to_add =
      match class_to_add with
      | Class
          (class_modifier_list, Name this_key, class_parent, class_elements_list)
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
                let is_virtual = is_virtual method_modifier_list in
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
                >> is_abstract_body
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
          let is_sealed = is_sealed class_modifier_list in
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
          list_map_monerr class_elements_list add_element ()
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
     fun class_t ->
      match class_t.parent_key with
      | None -> return class_t
      | Some parent_key -> (
          let parent_class = get_element_option ht parent_key in
          match parent_class with
          | None -> error "No parent class found"
          | Some parent when parent.is_sealed = false ->
              let new_parent =
                { parent with
                  children_keys= class_t.this_key :: parent.children_keys }
              in
              update_element_monerr ht parent_key new_parent
              >> return new_parent
          | Some _ -> error "Sealed class cannot be inherited" ) in
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

  let many_check_constructor class_t =
    let check_call_constructor : constructor_t -> unit t =
     fun constructor ->
      match constructor.call_constructor with
      | Some call_constructor ->
          if is_base_method call_constructor || is_this_method call_constructor
          then return ()
          else error "Cannot call method that is not a constructor"
      | None -> return () in
    list_map_monerr
      (convert_table_to_list class_t.constructors_table)
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
          if parent_method.is_virtual then
            return
              (Hashtbl.add children.methods_table parent_method.key
                 parent_method)
          else return ()
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
        | None -> error "Override modifier on not overriden method"
        | _ -> return () ) in
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
    >> many_check_constructor parent
    >> many_check_constructor children
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
