(*type cs_exception = {e_name: cs_type; e_message: string}*)

(* type cs_variable =
  { v_modifiers: cs_modifier list
  ; v_type: cs_type
  ; v_name: string
  ; v_value: cs_value option } *)

(* type cs_method =
   { m_modifiers: cs_modifier list
   ; m_type: cs_type
   ; m_name: string
   ; m_args: cs_expression list
   ; m_body: cs_statement list } *)

(* type cs_class =
  { c_modifiers: cs_modifier list
  ; c_name: string
  ; c_parent: cs_class option
  ; c_fields: cs_variable list
  ; c_methods: cs_method list
  ; c_classes: cs_class list } *)

type cs_modifier = Static | Public | Const | Virtual | Override | Abstract

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
  | ClassDecl of string * cs_expression list
  | ArrayDecl of cs_type * cs_expression option
  | CallMethod of cs_expression * cs_expression list
  | AccessByPoint of cs_expression * cs_expression
  | ArrayAccess of cs_expression * cs_expression
  | Assign of cs_expression * cs_expression
[@@deriving show]

and cs_statement =
  | Expression of cs_expression
  | StatementBlock of cs_statement list
  | If of cs_expression * cs_statement * cs_statement option
  | While of cs_expression * cs_statement (*option*)
  | For of
      cs_statement option
      * cs_expression option
      * cs_expression list
      * cs_statement (*option*)
  | Break
  | Continue
  | Return of cs_expression option
  | Throw of cs_expression
  | VariableDecl of cs_type * (cs_expression * cs_expression option) list
[@@deriving show]

and cs_field =
  | Field of
      cs_modifier list * cs_type * (cs_expression * cs_expression option) list
  | Method of
      cs_modifier list
      * cs_type
      * cs_expression
      * (cs_type * cs_expression) list
      * cs_statement option
  | Constructor of
      cs_modifier list
      * cs_expression
      * (cs_type * cs_expression) list
      * cs_statement
[@@deriving show]

and cs_class =
  | Class of
      cs_modifier list * cs_expression * cs_expression option * cs_field list
[@@deriving show]
