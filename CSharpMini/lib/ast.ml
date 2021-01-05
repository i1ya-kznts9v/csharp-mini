type modifiers =
  | Static
  | Public
  | Const
  | Virtual
  | Override
  | Abstract
  | Sealed
[@@deriving show {with_path= false}]

type types =
  | TInt
  | TVoid
  | TString
  | TClass of string
  | TArray of types
  | TBool
[@@deriving show {with_path= false}]

type values =
  | VInt of int
  | VBool of bool
  | VArray of array_references
  | VString of string
  | VVoid
  | VObjectReference of object_references
[@@deriving show {with_path= false}]

and field_references =
  { key: string
  ; field_type: types
  ; field_value: values
  ; is_const: bool
  ; assignments_count: int }

and object_references =
  | NullObjectReference
  | ObjectReference of
      { class_key: string
      ; field_references_table: (string, field_references) Hashtbl_impr.t
      ; number: int }

and array_references =
  | NullArrayReference
  | ArrayReference of {array_type: types; array_values: values list; number: int}

type names = Name of string [@@deriving show {with_path= false}]

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
[@@deriving show {with_path= false}]

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
  | VariableDecl of modifiers option * types * (names * expressions option) list
[@@deriving show {with_path= false}]

and fields =
  | Field of types * (names * expressions option) list
  | Method of types * names * (types * names) list * statements option
  | Constructor of
      names * (types * names) list * expressions option * statements
[@@deriving show {with_path= false}]

and classes =
  | Class of
      modifiers list * names * names option * (modifiers list * fields) list
[@@deriving show {with_path= false}]
