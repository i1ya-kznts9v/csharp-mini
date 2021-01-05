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

let get_object_fields = function
  | NullObjectReference -> raise (Invalid_argument "NullReferenceException")
  | ObjectReference
      {class_key= _; field_references_table= field_ref_table; number= _} ->
      field_ref_table

let get_object_info = function
  | NullObjectReference -> raise (Invalid_argument "NullReferenceException")
  | ObjectReference {class_key= key; field_references_table= table; number= num}
    ->
      (key, table, num)

let get_field_list = function Class (_, _, _, f_list) -> List.map snd f_list

let convert_element_pair_list = function
  | t, p_list -> List.map (fun p -> match p with s, f -> (t, s, f)) p_list

let get_variable_field_pairs_list_typed cd =
  List.concat
    (List.map convert_element_pair_list
       (List.filter_map
          (fun f ->
            match f with
            | Field (t, pair_list) -> Some (t, pair_list)
            | _ -> None)
          (get_field_list cd)))
