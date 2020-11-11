type csType =
  | TInt
  | TBool
  | TVoid
  | TChar
  | TString
  | TRef of string (*csClass*)
  | TArray of csType
  | TObject

type csValues =
  | VInt of int
  | VBool of bool
  | VChar of char
  | VArray of csValues list
  | VVoid
  | VNull
  | VObject
  | VRef
  | VString of string

type csModifier = Static | Public | Const | Virtual | Override | Abstract

type csException = { eName : csType; eMessage : string }

type csVariable = {
  vModifiers : csModifier list;
  vType : csType;
  vName : string;
  vValue : csValues option;
}

type csStatement =
  | Expr of csExpression
  | If of {
      condition : csExpression;
      thenStatement : csStatement list;
      elseStatement : csStatement list;
    }
  | While of { condition : csExpression; body : csStatement list }
  | For of {
      varDec : csStatement;
      condition : csExpression;
      afterBody : csStatement list;
      body : csStatement list;
    }
  | Break
  | Continue
  | Return of { result : csExpression }
  | VarDec of { vType : csType; vName : string; vValue : csValues option }
  | Throw of { exc : csException }

and csExpression =
  | Value of csValues
  | NumExpr of csNumericExpression
  | LogExpr of csLogicalExpression
  | CompExpr of csComparisonExpression
  | DecExpr of csDeclarationExpression
  | CallMethod of csExpression * csExpression list
  | Identifier of string
  | This
  | Base
  | Null
  | AccessByPoint of csExpression * csExpression
  | ArrayAccess of csExpression * csExpression list

and csDeclarationExpression =
  | ClassDec of { cName : string; cArgs : csExpression list }
  | ArrayDec of { aType : csType; aCount : csExpression }

and csComparisonExpression =
  | Equal of csExpression * csExpression
  | NotEqual of csExpression * csExpression
  | Less of csExpression * csExpression
  | More of csExpression * csExpression
  | LessOrEqual of csExpression * csExpression
  | MoreOrEqual of csExpression * csExpression

and csLogicalExpression =
  | And of csExpression * csExpression
  | Or of csExpression * csExpression
  | Not of csExpression

and csNumericExpression =
  | Add of csExpression * csExpression
  | Sub of csExpression * csExpression
  | Mult of csExpression * csExpression
  | Div of csExpression * csExpression
  | Mod of csExpression * csExpression
  | PostInc of csExpression
  | PostDec of csExpression
  | PrefInc of csExpression
  | PrefDec of csExpression

type csMethod = {
  mModifiers : csModifier list;
  mType : csType;
  mName : string;
  mArgs : csExpression list;
  mBody : csStatement list;
}

type csClass = {
  cModifiers : csModifier list;
  cName : string;
  cParent : csClass option;
  cFields : csVariable list;
  cMethods : csMethod list;
  cClasses : csClass list;
}
