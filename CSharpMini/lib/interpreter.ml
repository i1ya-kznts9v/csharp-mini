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
  {field_type: types; key: key_t; is_const: bool; subtree: expressions option}
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
  ; constructors_table: (key_t, constructor_r) Hashtbl.t }

let class_table : (key_t, class_r) Hashtbl.t = Hashtbl.create 1024
let convert_name_to_key = function Some (Name x) -> Some x | None -> None

let convert_table_to_list ht =
  Hashtbl.fold (fun _ value tl -> value :: tl) ht []

let convert_table_to_sequence = Hashtbl.to_seq_values
let get_element_option = Hashtbl.find_opt

module ClassLoader (M : MONADERROR) = struct
  open M

  let is_public = List.mem Public
  let is_static = List.mem Static
  let is_abstract = List.mem Abstract
  let is_const = List.mem Const
  let is_virtual = List.mem Virtual
  let is_override = List.mem Override

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
      ; key= "equals@@"
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
      ; key= "toString@@"
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
      ; parent_key= None } in
    Hashtbl.add methods_table equals.key equals ;
    Hashtbl.add methods_table to_string.key to_string ;
    Hashtbl.add ht object_hardcode.this_key object_hardcode ;
    return ht
end
