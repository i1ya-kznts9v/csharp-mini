type modifier = Public | Static | Final | Abstract | Override
[@@deriving show]

type type_t =
  | Int
  | Void
  | ClassName of string
  | String
  | Array of type_t
  | Object
[@@deriving show]

type value =
  | VBool of bool
  | VInt of int
  | VNull
  | VChar of char
  | VArray of value list
  | VVoid
  | VString of string
  | VClassName
  | VObject
[@@deriving show]

type name = Name of string [@@deriving show]

(* type jException = {jName: type_t ; message : string} 

type jVariable = {vModifiers : modifier list; isMutable : bool; varType : type_t; varName : string; value : value option}  *)

type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | PrefInc of expr
  | PrefDec of expr
  | PostInc of expr
  | PostDec of expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | Less of expr * expr
  | More of expr * expr
  | LessOrEqual of expr * expr
  | MoreOrEqual of expr * expr
  | ClassCreate of name * expr list (*new clName(argList)*)
  | ArrayCreateSized of type_t * expr (*new arrType[cntExpr]*)
  | ArrayCreateElements of type_t * expr list (*new arrType[] {expr, ... , expr}*)
  | CallMethod of expr * expr list (*this(...), super(...) ident(...)*)
  | Identifier of string
  | Const of value
  | This
  | Super
  | Null
  | FieldAccess of expr * expr
  | ArrayAccess of expr * expr (*arr_name[index]*)
  | Assign of expr * expr
[@@deriving show]

and stmt =
  | If of expr * stmt * stmt option (*cond * thenStat * elseStat*)
  | While of expr * stmt (* cond * body *)
  | For of stmt option * expr option * expr list * stmt (* varDec * expr * afterBody * body *)
  | Break
  | Continue
  | Return of expr option (* result *)
  | StmtBlock of stmt list
  | VarDec of type_t * (name * expr option) list
  | Expression of expr
  | Throw of expr
[@@deriving show]

and field =
  | Method of
      modifier list
      * type_t
      * name
      * (type_t * name) list
      (*List of pairs (type, identificator)*)
      * stmt option (*Statement block*)
  | VarField of modifier list * type_t * (name * expr option) list
  | Constructor of modifier list * name * (type_t * name) list * stmt
[@@deriving show]

and class_dec =
  | Class of
      modifier list
      * name (*class name*)
      * name option
      (*Parent class_name*)
      * field list
(* class body *) [@@deriving show]

(* type jMethod = {mModifiers: modifier list; mName : string; mRetType : type_t; mArgs : expr list; mBody : stmt list;}  

type jClass = {cModifiers: modifier list; cName : string; cFields : jVariable list; cMethods : jMethod list; cClasses : jClass list; cParent : jClass option}   *)
