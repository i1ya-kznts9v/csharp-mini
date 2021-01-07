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
  | ObjectReference {number= num; _} -> num

let get_object_key = function
  | NullObjectReference -> raise (Invalid_argument "NullReferenceException")
  | ObjectReference {class_key= key; _} -> key

let get_object_fields = function
  | NullObjectReference -> raise (Invalid_argument "NullReferenceException")
  | ObjectReference {field_references_table= field_ref_table; _} ->
      field_ref_table

let get_object_info = function
  | NullObjectReference -> raise (Invalid_argument "NullReferenceException")
  | ObjectReference {class_key= key; field_references_table= table; number= num}
    ->
      (key, table, num)

let get_array_info = function
  | NullArrayReference -> raise (Invalid_argument "NullRefenceException")
  | ArrayReference {array_type= arr_type; array_values= arr_val; number= arr_num}
    ->
      (arr_type, arr_val, arr_num)

let get_field_list = function
  | Class (_, _, _, field_list) -> List.map snd field_list

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

let get_type_by_value = function
  | VInt _ -> TInt
  | VBool _ -> TBool
  | VString _ -> TString
  | VVoid -> TVoid
  | VObjectReference NullObjectReference -> TClass "null"
  | VObjectReference (ObjectReference {class_key= ck; _}) -> TClass ck
  | VArray (ArrayReference {array_type= at; _}) -> TArray at
  | VArray NullArrayReference -> TArray TVoid

let update_array_value arr_v index new_val =
  match arr_v with
  | VArray (ArrayReference {array_type= at; array_values= v_list; number= an})
    ->
      let update_list_on_position pos list new_v =
        List.mapi (fun i old_v -> if i = pos then new_v else old_v) list in
      let check_value_type a_type new_val = a_type = get_type_by_value new_val in
      if check_value_type at new_val then
        update_list_on_position index v_list new_val
        |> fun new_list ->
        VArray
          (ArrayReference {array_type= at; array_values= new_list; number= an})
        (*Oh yes, this is exactly the exception that makes C# bad*)
      else raise (Failure "ArrayTypeMismatchException")
  | _ -> raise (Invalid_argument "Wrong value for array update")
