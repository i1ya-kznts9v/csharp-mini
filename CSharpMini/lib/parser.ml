open Opal
open Ast

(*Check fullness*)
let reserved =
  [ "true"; "false"; "if"; "else"; "for"; "while"; "public"; "const"; "static"
  ; "int"; "bool"; "string"; "void"; "char"; "Null"; "new"; "this"; "base"
  ; "vitual"; "override"; "abstract"; "namespace"; "using"; "do"; "return"
  ; "continue"; "brake"; "class" ]

let parens = between (token "(") (token ")")

(* let brackets = between (token "[") (token "]") *)

(*let braces = between (token "{") (token "}")*)

let digits = spaces >> many1 digit => implode
let integer = digits => int_of_string
let list_no_option list = match list with Some x -> x | None -> []
let list_with_option list = match list with [] -> None | x -> Some x

let modifier =
  choice
    [ token "public" >> return Public; token "static" >> return Static
    ; token "const" >> return Const; token "virtual" >> return Virtual
    ; token "override" >> return Override; token "abstract" >> return Abstract
    ]

module Expression = struct
  let null = token "Null" >> return Null

  let%test _ = parse null (LazyStream.of_string "          Null") = Some Null

  let base = token "base" >> return Base

  let%test _ = parse base (LazyStream.of_string "      base") = Some Base

  let this = token "this" >> return This

  let%test _ = parse this (LazyStream.of_string "         this") = Some This

  let int_value = integer >>= fun i -> return (Value (VInt i))

  let%test _ =
    parse int_value (LazyStream.of_string "            19012001")
    = Some (Value (VInt 19012001))

  (* let%test _ =
     parse int_value (LazyStream.of_string "            19012aa1") = None *)

  let string_value =
    let string_of_chars chars =
      let buffer = Buffer.create 16 in
      List.iter (Buffer.add_char buffer) chars ;
      Buffer.contents buffer in
    token "\""
    >> many (satisfy (fun c -> c <> '\"'))
    >>= fun list ->
    token "\"" >> return (Value (VString (string_of_chars list)))

  let%test _ =
    parse string_value (LazyStream.of_string "       \"Hello world!\"")
    = Some (Value (VString "Hello world!"))

  let false_value = token "false" >> return (Value (VBool false))
  let true_value = token "true" >> return (Value (VBool true))

  let ident =
    spaces >> letter <~> many alpha_num => implode
    >>= function s when List.mem s reserved -> mzero | s -> return s

  let identifier = ident >>= fun s -> return (Identifier s)

  let%test _ =
    parse identifier (LazyStream.of_string "       JetBrains")
    = Some (Identifier "JetBrains")

  let%test _ = parse identifier (LazyStream.of_string "     abstract") = None
  let%test _ = parse identifier (LazyStream.of_string "   1lya") = None

  let add_op = token "+" >> return (fun x y -> Add (x, y))
  let sub_op = token "-" >> return (fun x y -> Sub (x, y))
  let mult_op = token "*" >> return (fun x y -> Mult (x, y))
  let div_op = token "/" >> return (fun x y -> Div (x, y))
  let mod_op = token "%" >> return (fun x y -> Mod (x, y))
  let or_op = token "||" >> return (fun x y -> Or (x, y))
  let and_op = token "&&" >> return (fun x y -> And (x, y))
  let less_op = token "<" >> return (fun x y -> Less (x, y))
  let more_op = token ">" >> return (fun x y -> More (x, y))
  let less_or_equal_op = token "<=" >> return (fun x y -> LessOrEqual (x, y))
  let more_or_equal_op = token ">=" >> return (fun x y -> MoreOrEqual (x, y))
  let equal_op = token "==" >> return (fun x y -> Equal (x, y))
  let not_equal_op = token "!=" >> return (fun x y -> NotEqual (x, y))

  let atomaric =
    choice [identifier; int_value; string_value; true_value; false_value; null]

  let%test _ =
    parse atomaric (LazyStream.of_string "    false")
    = Some (Value (VBool false))

  let define_type =
    choice
      [ token "int" >> return TInt; token "string" >> return TString
      ; token "object" >> return TObject; token "void" >> return TVoid
      ; (ident >>= fun class_name -> return (TClass class_name)) ]

  let%test _ = parse define_type (LazyStream.of_string " object") = Some TObject

  let%test _ =
    parse define_type (LazyStream.of_string "   Ilya") = Some (TClass "Ilya")

  let define_array_type =
    let array_type t =
      token "["
      >> many (token ",")
      >>= fun dim_list ->
      token "]"
      >>
      match List.length dim_list with
      | 0 when t != TVoid -> return (TArray t)
      | _ -> mzero in
    choice
      [ token "int" >> array_type TInt; token "string" >> array_type TString
      ; token "object" >> array_type TObject; token "void" >> array_type TVoid
      ; (ident >>= fun class_name -> array_type (TClass class_name)) ]

  let%test _ =
    parse define_array_type (LazyStream.of_string "   string[,,]") = None

  let%test _ = parse define_array_type (LazyStream.of_string "   void[]") = None

  let%test _ =
    parse define_array_type (LazyStream.of_string "   int[]")
    = Some (TArray TInt)

  let%test _ =
    parse define_array_type (LazyStream.of_string "   JetBrains[]")
    = Some (TArray (TClass "JetBrains"))

  (* Functions below this point are arranged in ascending order of priority,
     but inside this functions, priorities are in descending order *)
  let rec expression input = or_expr input
  and or_expr input = (chainl1 and_expr or_op) input
  and and_expr input = (chainl1 comparison_expr and_op) input

  and comparison_expr input =
    (chainl1 add_sub_expr
       ( less_or_equal_op <|> more_or_equal_op <|> less_op <|> more_op
       <|> equal_op <|> not_equal_op ))
      input

  and add_sub_expr input = (chainl1 mult_div_mod_expr (add_op <|> sub_op)) input

  and mult_div_mod_expr input =
    (chainl1 unaric_expr (mult_op <|> div_op <|> mod_op)) input

  and unaric_expr input =
    choice
      [ (token "!" >> lexeme primary_expr >>= fun e -> return (Not e))
      ; ( token "-" >> lexeme primary_expr
        >>= fun e -> return (Sub (Value (VInt 0), e)) )
      ; (token "++" >> lexeme primary_expr >>= fun e -> return (PrefInc e))
      ; (token "--" >> lexeme primary_expr >>= fun e -> return (PrefDec e))
      ; (lexeme primary_expr >>= fun e -> token "++" >> return (PostInc e))
      ; (lexeme primary_expr >>= fun e -> token "--" >> return (PostDec e))
      ; primary_expr ]
      input

  and primary_expr input =
    ( class_creation <|> array_creation <|> assign <|> access_by_point
    <|> array_access <|> call_method <|> this <|> base <|> parens expression
    <|> atomaric )
      input

  and array_access input =
    ( this <|> parens array_creation <|> base <|> call_method <|> identifier
    >>= fun array_name ->
    define_array_index >>= fun index -> return (ArrayAccess (array_name, index))
    )
      input

  and access_by_point input =
    let fold_accesses acc el =
      match el with
      | ArrayAccess (identifier, index) ->
          ArrayAccess (AccessByPoint (acc, identifier), index)
      | other -> AccessByPoint (acc, other) in
    let called_parse =
      this <|> base <|> parens class_creation <|> array_access <|> call_method
      <|> identifier in
    ( called_parse
    >>= fun head ->
    many1 (token "." >> called_parse)
    => fun tl -> List.fold_left fold_accesses head tl )
      input

  and split_by_comma input = sep_by expression (token ",") input

  and define_array_index input =
    ( token "[" >> expression
    >>= fun index ->
    many (token ",")
    >>= fun commas ->
    match List.length commas with
    | 0 -> token "]" >> return index
    | _ -> token "]" >> mzero )
      input

  and call_method input =
    ( identifier <|> this <|> base
    >>= fun method_name ->
    token "(" >> split_by_comma
    >>= fun list_expr ->
    token ")" >> return (CallMethod (method_name, list_expr)) )
      input

  and class_creation input =
    ( token "new" >> identifier
    >>= fun class_name ->
    token "(" >> split_by_comma
    >>= fun list_expr ->
    token ")" >> return (ClassCreation (class_name, list_expr)) )
      input

  and array_creation input =
    ( token "new" >> define_type
    >>= fun array_type ->
    define_array_index
    >>= fun array_size -> return (ArrayCreation (array_type, array_size)) )
      input

  and assign input =
    let left = access_by_point <|> array_access <|> call_method <|> identifier in
    ( left
    >>= fun left ->
    token "=" >> expression >>= fun right -> return (Assign (left, right)) )
      input

  (*Priority location is over*)

  let%test _ =
    parse define_array_index (LazyStream.of_string " [ 14   ]")
    = Some (Value (VInt 14))

  let%test _ =
    parse define_array_index (LazyStream.of_string " [ 14, 19 , 99 ]") = None
end
