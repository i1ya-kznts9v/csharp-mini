type cs_modifier = Static | Public | Const | Virtual | Override | Abstract
[@@deriving show]

type cs_type =
  | TInt
  | TVoid
  | TString
  | TClass of string
  | TArray of cs_type
  | TObject
[@@deriving show]

type cs_value =
  | VInt of int
  | VBool of bool
  | VChar of char
  | VArray of cs_value list
  | VString of string
  | VVoid
  | VNull
  | VObject
  | VClass
[@@deriving show]

type cs_expression =
  | Add of cs_expression * cs_expression
  | Sub of cs_expression * cs_expression
  | Mult of cs_expression * cs_expression
  | Div of cs_expression * cs_expression
  | Mod of cs_expression * cs_expression
  | PostInc of cs_expression
  | PostDec of cs_expression
  | PrefInc of cs_expression
  | PrefDec of cs_expression
  | And of cs_expression * cs_expression
  | Or of cs_expression * cs_expression
  | Not of cs_expression
  | Equal of cs_expression * cs_expression
  | NotEqual of cs_expression * cs_expression
  | Less of cs_expression * cs_expression
  | More of cs_expression * cs_expression
  | LessOrEqual of cs_expression * cs_expression
  | MoreOrEqual of cs_expression * cs_expression
  | This
  | Base
  | Null
  | Value of cs_value
  | Identifier of string
  | ClassCreation of string * cs_expression list
  | ArrayCreationWithSize of cs_type * cs_expression
  | ArrayCreationWithElements of cs_type * cs_expression list
  | CallMethod of cs_expression * cs_expression list
  | AccessByPoint of cs_expression * cs_expression
  | ArrayAccess of cs_expression * cs_expression
  | Assign of cs_expression * cs_expression
[@@deriving show]

and cs_statement =
  | Expression of cs_expression
  | StatementBlock of cs_statement list
  | If of cs_expression * cs_statement * cs_statement option
  | While of cs_expression * cs_statement
  | For of
      cs_statement option
      * cs_expression option
      * cs_expression list
      * cs_statement
  | Break
  | Continue
  | Return of cs_expression option
  | Throw of cs_expression
  | VariableDecl of cs_type * (cs_expression * cs_expression option) list
[@@deriving show]

and cs_field =
  | Field of cs_modifier list * cs_type * (string * cs_expression option) list
  | Method of
      cs_modifier list
      * cs_type
      * string
      * (cs_type * string) list
      * cs_statement option
  | Constructor of
      cs_modifier list * string * (cs_type * string) list * cs_statement
[@@deriving show]

and cs_class =
  | Class of cs_modifier list * string * string option * cs_field list
[@@deriving show]
