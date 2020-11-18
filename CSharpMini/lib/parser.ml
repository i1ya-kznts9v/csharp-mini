open Opal
open Ast

let parens = between (token "(") (token ")")
let brackets = between (token "[") (token "]")

(*let braces = between (token "{") (token "}")*)

let listNoOption lst = match lst with Some x -> x | None -> []
let listWithOption lst = match lst with [] -> None | x -> Some x

module Expression = struct
  let base = token "base" >> return Base
  let this = token "this" >> return This

  (*TODO: check fullness*)
  let reserved =
    [ "true"; "false"; "if"; "else"; "for"; "while"; "public"; "const"; "static"
    ; "int"; "bool"; "string"; "void"; "char"; "Null"; "new"; "this"; "base"
    ; "vitual"; "override"; "abstract"; "namespace"; "using"; "do" ]

  let addOp = token "+" >> return (fun x y -> NumExpr (Add (x, y)))
  let subOp = token "-" >> return (fun x y -> NumExpr (Sub (x, y)))
  let multOp = token "*" >> return (fun x y -> NumExpr (Mult (x, y)))
  let divOp = token "/" >> return (fun x y -> NumExpr (Div (x, y)))
  let modOp = token "%" >> return (fun x y -> NumExpr (Mod (x, y)))
  let orOp = token "||" >> return (fun x y -> LogExpr (Or (x, y)))
  let andOp = token "&&" >> return (fun x y -> LogExpr (And (x, y)))
  let lessOp = token "<" >> return (fun x y -> CompExpr (Less (x, y)))
  let moreOp = token ">" >> return (fun x y -> CompExpr (More (x, y)))

  let lessOrEqualOp =
    token "<=" >> return (fun x y -> CompExpr (LessOrEqual (x, y)))

  let moreOrEqualOp =
    token ">=" >> return (fun x y -> CompExpr (MoreOrEqual (x, y)))

  let equalOp = token "==" >> return (fun x y -> CompExpr (Equal (x, y)))
  let notEqualOp = token "!=" >> return (fun x y -> CompExpr (NotEqual (x, y)))

  let identifier =
    spaces >> letter <~> many alpha_num => implode
    >>= function
    | x when List.mem x reserved -> mzero
    | x -> return x => fun x -> Identifier x

  let nullValue = token "Null" >> return (Value VNull)
  let falseValue = token "false" >> return (Value (VBool false))
  let trueValue = token "true" >> return (Value (VBool true))
  let digits = spaces >> many1 digit => implode
  let integer = digits => int_of_string
  let intValue = integer => fun x -> Value (VInt x)

  (*TODO: add smth*)
  let atomExpr =
    identifier <|> intValue <|> falseValue <|> trueValue <|> nullValue

  (*In ascending order of priority of operations*)
  let rec expression input = numExpr input
  and numExpr input = (chainl1 andExpr orOp) input
  and andExpr input = (chainl1 compExpr andOp) input

  and compExpr input =
    (chainl1 addExpr
       ( lessOp <|> moreOp <|> lessOrEqualOp <|> moreOrEqualOp <|> equalOp
       <|> notEqualOp ))
      input

  and addExpr input = (chainl1 multExpr (addOp <|> subOp)) input
  and multExpr input = (chainl1 unarExpr (multOp <|> divOp <|> modOp)) input

  (*TODO: Add inc dec*)
  and unarExpr input =
    choice
      [ (token "!" >> lexeme primExpr >>= fun s -> return (LogExpr (Not s)))
      ; ( token "-" >> lexeme primExpr
        >>= fun x -> return (NumExpr (Sub (Value (VInt 0), x))) ); primExpr ]
      input

  (*TODO: not working on a.b + 2*)
  and primExpr input =
    ( parens expression <|> accessByPoint <|> arrayAccess <|> callMethod
    <|> atomExpr )
      input

  and arrayAccess input =
    ( callMethod <|> identifier
    >>= fun aName ->
    many1 (brackets expression)
    >>= fun aIndexes -> return (ArrayAccess (aName, aIndexes)) )
      input

  and accessByPoint input =
    ( arrayAccess <|> callMethod <|> identifier
    >>= fun pName ->
    token "." >> lexeme expression
    >>= fun pCall -> return (AccessByPoint (pName, pCall)) )
      input

  and splitByComma input = sep_by expression (token ",") input

  and callMethod input =
    ( identifier
    >>= fun mName ->
    token "(" >> splitByComma
    >>= fun mArgs -> token ")" >> return (CallMethod (mName, mArgs)) )
      input
end
