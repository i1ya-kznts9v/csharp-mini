open Ast

let get_type_default_value = function
  | TInt -> VInt 0
  | TString -> VString ""
  | TClass _ -> VObjectReference NullObjectReference
  | TBool -> VBool false
  | TVoid -> VVoid
  | TArray _ -> VArray NullArrayReference

let get_object_value = function
  | VObjectReference value -> value
  | _ -> NullObjectReference

let get_object_number = function
  | NullObjectReference -> raise (Invalid_argument "NullReferenceException")
  | ObjectReference {class_key= _; field_references_table= _; number= num} ->
      num

let get_object_key = function
  | NullObjectReference -> raise (Invalid_argument "NullReferenceException")
  | ObjectReference {class_key= key; field_references_table= _; number= _} ->
      key

let get_object_info = function
  | NullObjectReference -> raise (Invalid_argument "NullReferenceException")
  | ObjectReference {class_key= key; field_references_table= table; number= num}
    ->
      (key, table, num)
