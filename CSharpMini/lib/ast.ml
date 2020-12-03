type modifiers = Static | Public | Const | Virtual | Override | Abstract
[@@deriving show]

type types =
  | TInt
  | TVoid
  | TString
  | TClass of string
  | TArray of types
  | TObject
[@@deriving show]

type values =
  | VInt of int
  | VBool of bool
  | VChar of char
  | VArray of values list
  | VString of string
  | VVoid
  | VNull
  | VObject
  | VClass
[@@deriving show]

type names = Name of string [@@deriving show]

type expressions =
  | Add of expressions * expressions
  | Sub of expressions * expressions
  | Mult of expressions * expressions
  | Div of expressions * expressions
  | Mod of expressions * expressions
  | PostInc of expressions
  | PostDec of expressions
  | PrefInc of expressions
  | PrefDec of expressions
  | And of expressions * expressions
  | Or of expressions * expressions
  | Not of expressions
  | Equal of expressions * expressions
  | NotEqual of expressions * expressions
  | Less of expressions * expressions
  | More of expressions * expressions
  | LessOrEqual of expressions * expressions
  | MoreOrEqual of expressions * expressions
  | This
  | Base
  | Null
  | Value of values
  | Identifier of string
  | ClassCreation of names * expressions list
  | ArrayCreationWithSize of types * expressions
  | ArrayCreationWithElements of types * expressions list
  | CallMethod of expressions * expressions list
  | AccessByPoint of expressions * expressions
  | ArrayAccess of expressions * expressions
  | Assign of expressions * expressions
[@@deriving show]

and statements =
  | Expression of expressions
  | StatementBlock of statements list
  | If of expressions * statements * statements option
  | While of expressions * statements
  | For of
      statements option * expressions option * expressions list * statements
  | Break
  | Continue
  | Return of expressions option
  | Throw of expressions
  | VariableDecl of types * (names * expressions option) list
[@@deriving show]

and fields =
  | Field of types * (names * expressions option) list
  | Method of types * names * (types * names) list * statements option
  | Constructor of
      names * (types * names) list * expressions option * statements
[@@deriving show]

and classes =
  | Class of
      modifiers list * names * names option * (modifiers list * fields) list
[@@deriving show]
