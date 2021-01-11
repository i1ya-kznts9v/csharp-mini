open Ast

let ( ++ ) left right =
  match (left, right) with
  | VInt x, VInt y -> VInt (x + y)
  | VString x, VString y -> VString (x ^ y)
  | VInt x, VString y -> VString (string_of_int x ^ y)
  | VString x, VInt y -> VString (x ^ string_of_int y)
  | _, _ -> raise (Invalid_argument "Incorrect argument types for adding")

let ( -- ) left right =
  match (left, right) with
  | VInt x, VInt y -> VInt (x - y)
  | _, _ -> raise (Invalid_argument "Incorrect argument types for subtraction!")

let ( ** ) left right =
  match (left, right) with
  | VInt x, VInt y -> VInt (x * y)
  | _, _ ->
      raise (Invalid_argument "Incorrect argument types for multiplication!")

let ( // ) left right =
  match (left, right) with
  | VInt _, VInt y when y = 0 -> raise Division_by_zero
  | VInt x, VInt y -> VInt (x / y)
  | _, _ -> raise (Invalid_argument "Incorrect argument types for division!")

let ( %% ) left right =
  match (left, right) with
  | VInt _, VInt y when y = 0 -> raise Division_by_zero
  | VInt x, VInt y -> VInt (x mod y)
  | _, _ ->
      raise (Invalid_argument "Incorrect argument types for mod operator!")

let ( >>> ) left right =
  match (left, right) with
  | VInt x, VInt y -> VBool (x > y)
  | _ -> raise (Invalid_argument "Incorrect type for ordering!")

let ( <<< ) left right =
  match (left, right) with
  | VInt x, VInt y -> VBool (x < y)
  | _ -> raise (Invalid_argument "Incorrect type for ordering!")

let ( <<== ) left right =
  match (left, right) with
  | VInt x, VInt y -> VBool (x <= y)
  | _ -> raise (Invalid_argument "Incorrect type for ordering!")

let ( >>== ) left right =
  match (left, right) with
  | VInt x, VInt y -> VBool (x >= y)
  | _ -> raise (Invalid_argument "Incorrect type for ordering!")

let ( &&& ) left right =
  match (left, right) with
  | VBool x, VBool y -> VBool (x && y)
  | _, _ -> raise (Invalid_argument "Incorrect types for && operator!")

let ( ||| ) left right =
  match (left, right) with
  | VBool x, VBool y -> VBool (x || y)
  | _, _ -> raise (Invalid_argument "Incorrect types for || operator!")

let not_op = function
  | VBool x -> VBool (not x)
  | _ -> raise (Invalid_argument "Incorrect types for NOT operator!")

let ( === ) left right =
  match (left, right) with
  | VInt x, VInt y -> VBool (x = y)
  | VBool x, VBool y -> VBool (x = y)
  | VVoid, VVoid -> VBool true
  | VString s, VString t -> VBool (s = t)
  | VClass x, VClass y -> (
    match (x, y) with
    | ObjNull, ObjNull -> VBool true
    | ObjNull, _ | _, ObjNull -> VBool false
    | ObjRef {number= xn; _}, ObjRef {number= yn; _} -> VBool (xn = yn) )
  | _ -> raise (Invalid_argument "Incorrect types for equality!")

let ( !=! ) left right = not_op (left === right)
