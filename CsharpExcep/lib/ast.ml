type data_type = Int | Bool | CsClass of string | Void | String
[@@deriving show {with_path= false}]

type value =
  | VInt of int
  | VBool of bool
  | VVoid
  | VNull
  | VString of string
  | VClass of obj_ref
[@@deriving show {with_path= false}]

and field_ref =
  { key: string
  ; f_type: data_type
  ; f_value: value
  ; is_const: bool
  ; assignment_count: int }

and obj_ref =
  | ObjNull
  | ObjRef of
      { class_key: string
      ; parent_key: string option
      ; class_table: (string, field_ref) Hashtbl_der.t
      ; number: int }

type modifier = Public | Static | Override | Const
[@@deriving show {with_path= false}]

type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | PostInc of expr (*x++*)
  | PostDec of expr (*x--*)
  | PrefInc of expr (*++x*)
  | PrefDec of expr (*--x*)
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | Less of expr * expr
  | More of expr * expr
  | LessOrEqual of expr * expr
  | MoreOrEqual of expr * expr
  | Null
  | Access of expr * expr
  | ConstExpr of value
  | IdentVar of string
  | ClassCreate of string * expr list (*first - name, other - args list*)
  | CallMethod of string * expr list
  | Assign of expr * expr
[@@deriving show {with_path= false}]

and statement =
  | For of statement option * expr option * expr list * statement (*because you can write for(int i = 0, j = 5; i < 4; i++, j--)*)
  | If of expr * statement * statement option (*statement option is "else"*)
  | While of expr * statement
  | Break
  | Continue
  | Return of expr option
  | VarDeclare of modifier option * data_type * (string * expr option) list
  | Expression of expr
  | Throw of expr
  | StatementBlock of statement list
  | Try of
      statement (*it is the body of try*)
      * ((data_type * expr option) option * expr option * statement) list
      (*it is list of catches: (data_type * expr option) this is a pair of the following form (DivideByZeroException ex),
        second param is option because you can write (DivideByZeroException) without ex
          next expr option is a filter (when (y==0 && x == 0)), statement is the body of catch*)
      * statement option (*it is the finally body*)
  | Print of expr
[@@deriving show {with_path= false}]

and field =
  | VariableField of data_type * (string * expr option) list (*example: static int a = 3, b*)
  | Method of data_type * string * (data_type * string) list * statement
  | Constructor of string * (data_type * string) list * statement
[@@deriving show {with_path= false}]

and cs_class =
  | Class of
      modifier list
      * string (*name*)
      * string option
      (*parent class*)
      * (modifier list * field) list
[@@deriving show {with_path= false}]
