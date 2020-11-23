open Opal
open Ast

(*Check fullness*)
let reserved =
  [ "true"; "false"; "if"; "else"; "for"; "while"; "public"; "const"; "static"
  ; "int"; "bool"; "string"; "void"; "char"; "null"; "new"; "this"; "base"
  ; "vitual"; "override"; "abstract"; "namespace"; "using"; "do"; "return"
  ; "continue"; "brake"; "class" ]

let parens = between (token "(") (token ")")
let braces = between (token "{") (token "}")

(* let brackets = between (token "[") (token "]") *)
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
  let null = token "null" >> return Null

  let%test _ = parse null (LazyStream.of_string "          null") = Some Null

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

  (*let define_type =
    choice
      [ token "int" >> return TInt; token "string" >> return TString
      ; token "object" >> return TObject; token "void" >> return TVoid
      ; (ident >>= fun class_name -> return (TClass class_name)) ]*)

  let define_type =
    let check_array t =
      option ' ' (exactly '[')
      >>= fun bracket ->
      match bracket with
      | '[' -> (
          many (token ",")
          >>= fun dim_list ->
          token "]"
          >>
          match List.length dim_list with
          | 0 when t != TVoid -> return (Some true)
          | _ -> return (Some false) )
      | _ -> return None in
    let type_decision t =
      check_array t
      >>= fun result ->
      match result with
      | Some true -> return (TArray t)
      | Some false -> mzero
      | None -> return t in
    choice
      [ token "int" >> type_decision TInt
      ; token "string" >> type_decision TString
      ; token "object" >> type_decision TObject
      ; token "void" >> type_decision TVoid
      ; (ident >>= fun class_name -> type_decision (TClass class_name)) ]

  let%test _ = parse define_type (LazyStream.of_string " object") = Some TObject

  let%test _ =
    parse define_type (LazyStream.of_string "   Ilya  ") = Some (TClass "Ilya")

  let%test _ = parse define_type (LazyStream.of_string "   string[,,]") = None
  let%test _ = parse define_type (LazyStream.of_string "   void[]") = None

  let%test _ =
    parse define_type (LazyStream.of_string "   int[]") = Some (TArray TInt)

  let%test _ =
    parse define_type (LazyStream.of_string "   JetBrains[]   ")
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
    ( token "new" >> ident
    >>= fun class_name ->
    token "(" >> split_by_comma
    >>= fun list_expr ->
    token ")" >> return (ClassCreation (class_name, list_expr)) )
      input

  and array_creation input =
    ( token "new" >> define_type
    >>= fun array_type ->
    choice
      [ ( token "[]" >> token "{"
        >> sep_by1 expression (token ",")
        >>= fun elements_list ->
        return (ArrayCreationWithElements (array_type, elements_list)) )
      ; ( define_array_index
        >>= fun array_size ->
        return (ArrayCreationWithSize (array_type, array_size)) ) ] )
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

module Statement = struct
  open Expression

  let break_stat = token "break" >> token ";" >> return Break

  let%test _ = parse break_stat (LazyStream.of_string "break;") = Some Break

  let continue_stat = token "continue" >> token ";" >> return Continue

  let%test _ =
    parse continue_stat (LazyStream.of_string "continue;") = Some Continue

  let return_stat =
    token "return"
    >> choice
         [ (expression >>= fun ret -> token ";" >> return (Return (Some ret)))
         ; token ";" >> return (Return None) ]

  let%test _ =
    parse return_stat (LazyStream.of_string "return;") = Some (Return None)

  let%test _ =
    parse return_stat (LazyStream.of_string "return 0;")
    = Some (Return (Some (Value (VInt 0))))

  let%test _ =
    parse return_stat (LazyStream.of_string "return x >= y;")
    = Some (Return (Some (MoreOrEqual (Identifier "x", Identifier "y"))))

  let expression_stat =
    expression >>= fun expr -> token ";" >> return (Expression expr)

  let%test _ =
    parse expression_stat (LazyStream.of_string "Call();")
    = Some (Expression (CallMethod (Identifier "Call", [])))

  let%test _ =
    parse expression_stat (LazyStream.of_string "--m;")
    = Some (Expression (PrefDec (Identifier "m")))

  let rec statement input =
    choice
      [ variable_decl; break_stat; continue_stat; return_stat; if_stat
      ; while_stat; for_stat; throw_stat; expression_stat; statement_block ]
      input

  and if_stat input =
    ( token "if" >> parens expression
    >>= fun condition ->
    statement
    >>= fun if_body ->
    choice
      [ ( token "else" >> statement
        >>= fun else_body -> return (If (condition, if_body, Some else_body)) )
      ; return (If (condition, if_body, None)) ] )
      input

  and statement_block input =
    ( braces (sep_by statement spaces)
    >>= fun block_stat -> return (StatementBlock block_stat) )
      input

  and while_stat input =
    ( token "while" >> parens expression
    >>= fun condition ->
    statement >>= fun while_body -> return (While (condition, while_body)) )
      input

  and variable_decl input =
    let identifier_and_value =
      identifier
      >>= fun variable_name ->
      token "=" >> expression
      >>= (fun variable_value -> return (variable_name, Some variable_value))
      <|> return (variable_name, None) in
    ( define_type
    >>= fun variable_type ->
    sep_by1 identifier_and_value (token ",")
    >>= fun variable_decl_list ->
    token ";" >> return (VariableDecl (variable_type, variable_decl_list)) )
      input

  (*and for_stat input =*)
  and throw_stat =
    token "throw" >> expression >>= fun except -> return (Throw except)
end
