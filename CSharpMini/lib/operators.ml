open Ast

let ( ++ ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VInt (left_value + right_value)
  | VString left_value, VString right_value -> VString (left_value ^ right_value)
  | VInt left_value, VString right_value ->
      VString (string_of_int left_value ^ right_value)
  | VString left_value, VInt right_value ->
      VString (left_value ^ string_of_int right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < + >")

let ( -- ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VInt (left_value - right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < - >")

let ( ** ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VInt (left_value * right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < * >")

let ( // ) left right =
  match (left, right) with
  | VInt _, VInt right_value when right_value = 0 -> raise Division_by_zero
  | VInt left_value, VInt right_value -> VInt (left_value / right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < / >")

let ( %% ) left right =
  match (left, right) with
  | VInt _, VInt right_value when right_value = 0 -> raise Division_by_zero
  | VInt left_value, VInt right_value -> VInt (left_value mod right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < % >")

let ( >>> ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VBool (left_value > right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < (>) >")

let ( <<< ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VBool (left_value < right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < (<) >")

let ( >>== ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VBool (left_value >= right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < (>=) >")

let ( <<== ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VBool (left_value <= right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < (<=) >")

let ( &&& ) left right =
  match (left, right) with
  | VBool left_value, VBool right_value -> VBool (left_value && right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < && >")

let ( ||| ) left right =
  match (left, right) with
  | VBool left_value, VBool right_value -> VBool (left_value || right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < || >")

let ( !!! ) = function
  | VBool value -> VBool (not value)
  | _ -> raise (Invalid_argument "Wrong types for < ! >")

let ( === ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VBool (left_value = right_value)
  | VBool left_value, VBool right_value -> VBool (left_value = right_value)
  | VVoid, VVoid -> VBool true
  | VString left_value, VString right_value -> VBool (left_value = right_value)
  | VObjectReference left_value, VObjectReference right_value -> (
    match (left_value, right_value) with
    | NullObjectReference, NullObjectReference -> VBool true
    | NullObjectReference, _ | _, NullObjectReference -> VBool false
    | ( ObjectReference
          {class_key= _; field_references_table= _; number= left_number}
      , ObjectReference
          {class_key= _; field_references_table= _; number= right_number} ) ->
        VBool (left_number = right_number) )
  | VArray left_value, VArray right_value -> VBool (left_value = right_value)
  | _ -> raise (Invalid_argument "Wrong types in < == >")

let ( !=! ) left right = !!!(left === right)
