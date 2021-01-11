open Ast

let get_obj_value = function VClass o -> o | _ -> ObjNull

let get_default_value = function
  | Int -> VInt 0
  | String -> VString ""
  | CsClass _ -> VClass ObjNull
  | Bool -> VBool false
  | Void -> VVoid

let get_obj_num = function
  | ObjNull -> raise (Invalid_argument "NullReferenceException")
  | ObjRef {number= n; _} -> n

let get_obj_info = function
  | ObjNull -> raise (Invalid_argument "NullReferenceException")
  | ObjRef {class_key= key; class_table= table; number= n; _} -> (key, table, n)

let get_obj_fields = function
  | ObjNull -> raise (Invalid_argument "NullReferenceException")
  | ObjRef {class_table= frt; _} -> frt

let get_field_list = function Class (_, _, _, f_list) -> List.map snd f_list

let convert_pair = function
  | t, p_list -> List.map (fun p -> match p with s, f -> (t, s, f)) p_list

let get_field_pairs in_class =
  List.concat
    (List.map convert_pair
       (List.filter_map
          (fun field ->
            match field with
            | VariableField (t_f, pair_list) -> Some (t_f, pair_list)
            | _ -> None)
          (get_field_list in_class)))
