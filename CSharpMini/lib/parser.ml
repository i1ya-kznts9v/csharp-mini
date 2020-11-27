open Ast
open Opal

let apply p s = parse p (LazyStream.of_string s)

module Expression = struct
  let parens parser =
    token "(" >> parser >>= fun result -> token ")" >> return result

  let digits = spaces >> many1 digit => implode
  let integer = digits => int_of_string

  let%test _ = apply integer " 1901" = Some 1901

  let null = token "null" >> return Null

  let%test _ = apply null "          null" = Some Null

  let base = token "base" >> return Base

  let%test _ = apply base "      base" = Some Base

  let this = token "this" >> return This

  let%test _ = apply this "         this" = Some This

  let int_value = integer >>= fun i -> return (Value (VInt i))

  let%test _ =
    apply int_value "            19012001" = Some (Value (VInt 19012001))

  (* let%test _ =
     apply int_value "            19012aa1" = None *)

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
    apply string_value "       \"Hello world!\""
    = Some (Value (VString "Hello world!"))

  let false_value = token "false" >> return (Value (VBool false))
  let true_value = token "true" >> return (Value (VBool true))

  let reserved =
    [ "true"; "false"; "if"; "else"; "for"; "while"; "public"; "const"; "static"
    ; "int"; "bool"; "string"; "void"; "char"; "null"; "new"; "this"; "base"
    ; "vitual"; "override"; "abstract"; "namespace"; "using"; "do"; "return"
    ; "continue"; "brake"; "class" ]

  let ident =
    spaces >> letter <~> many alpha_num => implode
    >>= function s when List.mem s reserved -> mzero | s -> return s

  let identifier = ident >>= fun s -> return (Identifier s)

  let%test _ =
    apply identifier "       JetBrains" = Some (Identifier "JetBrains")

  let%test _ = apply identifier "     abstract" = None
  let%test _ = apply identifier "   1lya" = None

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

  let atomic =
    choice [identifier; int_value; string_value; true_value; false_value; null]

  let%test _ = apply atomic "    false" = Some (Value (VBool false))

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
    <|> atomic )
      input

  and array_access input =
    ( this <|> parens array_creation <|> base <|> call_method <|> identifier
    >>= fun array_name ->
    token "[" >> define_array_index
    >>= fun index ->
    match index with
    | Null -> token "]" >> mzero
    | _ -> token "]" >> return (ArrayAccess (array_name, index)) )
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
    ( option Null expression
    >>= fun index ->
    many (token ",")
    >>= fun commas ->
    match List.length commas with 0 -> return index | _ -> mzero )
      input

  and define_type_and_index_option input =
    let check_array t input =
      ( option ' ' (exactly '[')
      >>= fun bracket ->
      match bracket with
      | '[' -> (
          define_array_index
          >>= fun index ->
          token "]"
          >>
          match t with
          | TInt when index != Null -> return (Some index)
          | TString when index != Null -> return (Some index)
          | TObject when index != Null -> return (Some index)
          | TClass _ when index != Null -> return (Some index)
          | TVoid -> mzero
          | _ -> return (Some Null) )
      | _ -> return None )
        input in
    let type_and_index_decision t input =
      ( check_array t
      >>= fun result ->
      match result with
      | Some Null -> return (TArray t, None)
      | Some index -> return (TArray t, Some index)
      | None -> return (t, None) )
        input in
    (choice
       [ token "int" >> type_and_index_decision TInt
       ; token "string" >> type_and_index_decision TString
       ; token "object" >> type_and_index_decision TObject
       ; ( ident
         >>= fun class_name -> type_and_index_decision (TClass class_name) )
       ; token "void" >> type_and_index_decision TVoid ])
      input

  and call method_name input =
    ( parens split_by_comma
    >>= fun list_expr -> return (CallMethod (method_name, list_expr)) )
      input

  and call_constructor input =
    (this <|> base >>= fun constr_name -> call constr_name) input

  and call_method input =
    (identifier >>= fun method_name -> call method_name) input

  and class_creation input =
    ( token "new" >> identifier
    >>= fun class_name ->
    parens split_by_comma
    >>= fun list_expr -> return (ClassCreation (class_name, list_expr)) )
      input

  and array_creation input =
    ( token "new" >> define_type_and_index_option
    >>= fun (array_type, array_size) ->
    match array_type with
    | TArray array_type ->
        choice
          [ ( token "{"
            >> sep_by expression (token ",")
            >>= fun array_elements_list ->
            token "}"
            >> return
                 (ArrayCreationWithElements (array_type, array_elements_list))
            )
          ; ( match array_size with
            | None -> mzero
            | Some array_size ->
                return (ArrayCreationWithSize (array_type, array_size)) ) ]
    | _ -> mzero )
      input

  and assign input =
    let left = access_by_point <|> array_access <|> call_method <|> identifier in
    ( left
    >>= fun left ->
    token "=" >> expression >>= fun right -> return (Assign (left, right)) )
      input

  (*Priority location is over*)

  let%test _ = apply define_type_and_index_option "   string[,,]" = None
  let%test _ = apply define_type_and_index_option "   void[]" = None

  let%test _ =
    apply define_type_and_index_option " object" = Some (TObject, None)

  let%test _ =
    apply define_type_and_index_option "   Ilya  " = Some (TClass "Ilya", None)

  let%test _ =
    apply define_type_and_index_option "   int[]" = Some (TArray TInt, None)

  let%test _ =
    apply define_type_and_index_option "   int[99]"
    = Some (TArray TInt, Some (Value (VInt 99)))

  let%test _ =
    apply define_type_and_index_option "   JetBrains[]   "
    = Some (TArray (TClass "JetBrains"), None)

  let%test _ =
    apply define_type_and_index_option "   JetBrains[10]   "
    = Some (TArray (TClass "JetBrains"), Some (Value (VInt 10)))

  let%test _ = apply define_array_index "  14   " = Some (Value (VInt 14))
  let%test _ = apply define_array_index "  14, 19 ,99 " = None
end

module Statement = struct
  open Expression

  let break_stat = token "break" >> token ";" >> return Break

  let%test _ = apply break_stat "break;" = Some Break

  let continue_stat = token "continue" >> token ";" >> return Continue

  let%test _ = apply continue_stat "continue;" = Some Continue

  let return_stat =
    token "return"
    >> choice
         [ (expression >>= fun ret -> token ";" >> return (Return (Some ret)))
         ; token ";" >> return (Return None) ]

  let%test _ = apply return_stat "return;" = Some (Return None)

  let%test _ =
    apply return_stat "return 0;" = Some (Return (Some (Value (VInt 0))))

  let%test _ =
    apply return_stat "return x >= y;"
    = Some (Return (Some (MoreOrEqual (Identifier "x", Identifier "y"))))

  let expression_stat =
    expression >>= fun expr -> token ";" >> return (Expression expr)

  let%test _ =
    apply expression_stat "Call();"
    = Some (Expression (CallMethod (Identifier "Call", [])))

  let%test _ =
    apply expression_stat "--m;" = Some (Expression (PrefDec (Identifier "m")))

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
    ( token "{" >> sep_by statement spaces
    >>= fun block_stat -> token "}" >> return (StatementBlock block_stat) )
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
    ( define_type_and_index_option
    >>= fun (variable_type, variable_index) ->
    match variable_index with
    | Some _ -> mzero
    | None ->
        sep_by1 identifier_and_value (token ",")
        >>= fun variable_decl_list ->
        token ";" >> return (VariableDecl (variable_type, variable_decl_list))
    )
      input

  and for_stat input =
    ( token "for"
    >> parens
         ( choice
             [ (statement >>= fun stat -> return (Some stat))
             ; token ";" >> return None ]
         >>= fun declaration ->
         choice
           [ (expression >>= fun expr -> token ";" >> return (Some expr))
           ; token ";" >> return None ]
         >>= fun condition ->
         sep_by expression (token ",")
         >>= fun after_list -> return (declaration, condition, after_list) )
    >>= fun (declaration, condition, after_list) ->
    statement
    >>= fun body -> return (For (declaration, condition, after_list, body)) )
      input

  and throw_stat =
    token "throw" >> expression >>= fun except -> return (Throw except)
end

module Class = struct
  open Expression
  open Statement

  let modifier =
    choice
      [ token "public" >> return Public; token "static" >> return Static
      ; token "const" >> return Const; token "virtual" >> return Virtual
      ; token "override" >> return Override; token "abstract" >> return Abstract
      ]

  let%test _ =
    apply (many modifier) "public static abstract"
    = Some [Public; Static; Abstract]

  let parameter =
    define_type_and_index_option
    >>= fun (parameter_type, parameter_index) ->
    match parameter_index with
    | Some _ -> mzero
    | None ->
        identifier
        >>= fun parameter_identifier ->
        return (parameter_type, parameter_identifier)

  let%test _ =
    apply (sep_by parameter (token ",")) "int a, string b, object o"
    = Some
        [ (TInt, Identifier "a"); (TString, Identifier "b")
        ; (TObject, Identifier "o") ]

  let method_decl =
    many modifier
    >>= fun method_modifier_list ->
    define_type_and_index_option
    >>= fun (method_type, method_index) ->
    match method_index with
    | Some _ -> mzero
    | None ->
        identifier
        >>= fun method_name ->
        parens (sep_by parameter (token ","))
        >>= fun method_parameter_list ->
        choice
          [ ( statement_block
            >>= fun method_statement_block ->
            return
              (Method
                 ( method_modifier_list
                 , method_type
                 , method_name
                 , method_parameter_list
                 , Some method_statement_block )) )
          ; token ";"
            >> return
                 (Method
                    ( method_modifier_list
                    , method_type
                    , method_name
                    , method_parameter_list
                    , None )) ]

  let%test _ =
    apply method_decl "public int Sum(int a, string b) {}"
    = Some
        (Method
           ( [Public]
           , TInt
           , Identifier "Sum"
           , [(TInt, Identifier "a"); (TString, Identifier "b")]
           , Some (StatementBlock []) ))

  let%test _ =
    apply method_decl "public abstract int Sum(int a, string b);"
    = Some
        (Method
           ( [Public; Abstract]
           , TInt
           , Identifier "Sum"
           , [(TInt, Identifier "a"); (TString, Identifier "b")]
           , None ))

  let constructor_decl =
    many modifier
    >>= fun constr_modifier_list ->
    identifier
    >>= fun constr_name ->
    parens (sep_by parameter (token ","))
    >>= fun constr_parameter_list ->
    choice
      [ ( token ":" >> call_constructor
        >>= fun constr_name -> return (Some constr_name) ); return None ]
    >>= fun call_constr_name ->
    statement_block
    >>= fun constr_statement_block ->
    return
      (Constructor
         ( constr_modifier_list
         , constr_name
         , constr_parameter_list
         , call_constr_name
         , constr_statement_block ))

  let%test _ =
    apply constructor_decl "public Win(object o) {}"
    = Some
        (Constructor
           ( [Public]
           , Identifier "Win"
           , [(TObject, Identifier "o")]
           , None
           , StatementBlock [] ))

  let%test _ =
    apply constructor_decl "public Win(object o, string m) : base(m) {}"
    = Some
        (Constructor
           ( [Public]
           , Identifier "Win"
           , [(TObject, Identifier "o"); (TString, Identifier "m")]
           , Some (CallMethod (Base, [Identifier "m"]))
           , StatementBlock [] ))

  let field_decl =
    let variable_decl =
      identifier
      >>= fun field_name ->
      choice
        [ ( token "=" >> expression
          >>= fun field_value -> return (field_name, Some field_value) )
        ; return (field_name, None) ] in
    many modifier
    >>= fun field_modifier_list ->
    define_type_and_index_option
    >>= fun (field_type, field_index) ->
    match field_index with
    | Some _ -> mzero
    | None ->
        sep_by variable_decl (token ",")
        >>= fun variable_decl_list ->
        token ";"
        >> return (Field (field_modifier_list, field_type, variable_decl_list))

  let%test _ =
    apply field_decl "public static int test;"
    = Some (Field ([Public; Static], TInt, [(Identifier "test", None)]))

  let%test _ =
    apply field_decl "public const string mega = \"JB\";"
    = Some
        (Field
           ( [Public; Const]
           , TString
           , [(Identifier "mega", Some (Value (VString "JB")))] ))

  let class_element = field_decl <|> constructor_decl <|> method_decl

  let class_decl =
    many modifier
    >>= fun class_modifier_list ->
    token "class" >> identifier
    >>= fun class_name ->
    choice
      [ ( token ":" >> identifier
        >>= fun class_parent_name -> return (Some class_parent_name) )
      ; return None ]
    >>= fun class_parent_name ->
    token "{"
    >> sep_by class_element spaces
    >>= fun class_element_list ->
    token "}"
    >> return
         (Class
            ( class_modifier_list
            , class_name
            , class_parent_name
            , class_element_list ))

  let%test _ =
    apply class_decl "public abstract class JetBrains : Company{}"
    = Some
        (Class
           ( [Public; Abstract]
           , Identifier "JetBrains"
           , Some (Identifier "Company")
           , [] ))

  let%test _ =
    apply class_decl "public static class JetBrains\n{}"
    = Some (Class ([Public; Static], Identifier "JetBrains", None, []))
end

let parser = many Class.class_decl
