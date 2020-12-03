open Opal
open Ast

let reserved =
  [
    "true";
    "false";
    "if";
    "else";
    "while";
    "public";
    "final";
    "static";
    "int";
    "String";
    "void";
    "boolean";
    "for";
    "null";
    "new";
    "this";
    "super";
    "class";
    "break";
    "continue";
    "extends";
    "return";
    "@Override";
  ]

let apply p s = parse p (LazyStream.of_string s)

let parens = between (token "(") (token ")")

let brackets = between (token "[") (token "]")

let digits = spaces >> many1 digit => implode

let integer = digits => int_of_string

let modifier input =
  choice
    [
      token "@Override" >> return Override;
      token "public" >> return Public;
      token "static" >> return Static;
      token "final" >> return Final;
      token "abstract" >> return Abstract;
    ]
    input

let ident =
  spaces >> letter <~> many alpha_num => implode >>= function
  | s when List.mem s reserved -> mzero
  | s -> return s

let name = ident >>= fun name -> return (Name name)

module Expr = struct
  let null = token "null" >> return Null

  let%test _ = apply null "null" = Some Null

  let%test _ = apply null "   null" = Some Null

  let super = token "super" >> return Super

  let%test _ = apply super "super" = Some Super

  let%test _ = apply super "   super" = Some Super

  let this = token "this" >> return This

  let%test _ = apply this "this" = Some This

  let%test _ = apply this "   this" = Some This

  let constInt = integer >>= fun n -> return (Const (VInt n))

  let%test _ = apply constInt "100500" = Some (Const (VInt 100500))

  let%test _ = apply constInt "    100500" = Some (Const (VInt 100500))

  let constString =
    let string_of_chars chars =
      let buf = Buffer.create 16 in
      List.iter (Buffer.add_char buf) chars;
      Buffer.contents buf
    in
    token "\"" >> many (satisfy (fun c -> c <> '\"')) >>= fun list ->
    token "\"" >> return (Const (VString (string_of_chars list)))

  let%test _ =
    apply constString "\"hello world!\"" = Some (Const (VString "hello world!"))

  let identifier = ident => fun s -> Identifier s

  let%test _ = apply identifier "IdentSample" = Some (Identifier "IdentSample")

  let%test _ = apply identifier "super" = None

  let%test _ = apply identifier "123bob" = None

  let add_op = token "+" >> return (fun x y -> Add (x, y))

  let sub_op = token "-" >> return (fun x y -> Sub (x, y))

  let mul_op = token "*" >> return (fun x y -> Mult (x, y))

  let div_op = token "/" >> return (fun x y -> Div (x, y))

  let mod_op = token "%" >> return (fun x y -> Mod (x, y))

  let or_op = token "||" >> return (fun x y -> Or (x, y))

  let and_op = token "&&" >> return (fun x y -> And (x, y))

  let l_op = token "<" >> return (fun x y -> Less (x, y))

  let m_op = token ">" >> return (fun x y -> More (x, y))

  let le_op = token "<=" >> return (fun x y -> LessOrEqual (x, y))

  let me_op = token ">=" >> return (fun x y -> MoreOrEqual (x, y))

  let eq_op = token "==" >> return (fun x y -> Equal (x, y))

  let neq_op = token "!=" >> return (fun x y -> NotEqual (x, y))

  let atomic =
    identifier <|> constInt <|> constString
    <|> (token "true" >> return (Const (VBool true)))
    <|> (token "false" >> return (Const (VBool false)))
    <|> null

  let%test _ = apply atomic "true" = Some (Const (VBool true))

  let type_spec_array =
    let parse_arr_or_type t =
      many (token "[]") >>= fun br_list ->
      match List.length br_list with
      | 0 -> return t
      | 1 when t <> Void -> return (Array t)
      | _ -> mzero
    in
    choice
      [
        token "int" >> parse_arr_or_type Int;
        token "String" >> parse_arr_or_type String;
        token "void" >> parse_arr_or_type Void;
        (ident >>= fun class_name -> parse_arr_or_type (ClassName class_name));
      ]

  let%test _ = apply type_spec_array "int[][][]" = None

  let%test _ = apply type_spec_array "int" = Some Int

  let%test _ = apply type_spec_array "int[]" = Some (Array Int)

  let%test _ = apply type_spec_array "void[]" = None

  let%test _ = apply type_spec_array "Car[]" = Some (Array (ClassName "Car"))

  let type_spec =
    choice
      [
        token "int" >> return Int;
        token "String" >> return String;
        token "void" >> return Void;
        (ident >>= fun class_name -> return (ClassName class_name));
      ]

  let%test _ = apply type_spec "int" = Some Int

  let%test _ = apply type_spec "   void" = Some Void

  let%test _ = apply type_spec "Car" = Some (ClassName "Car")

  let rec expression input = choice [ numeric ] input

  and numeric input = (chainl1 and_expr or_op) input

  and and_expr input = (chainl1 test_expr and_op) input

  and test_expr input =
    (chainl1 add_expr (le_op <|> me_op <|> l_op <|> m_op <|> eq_op <|> neq_op))
      input

  and add_expr input = (chainl1 mult_expr (add_op <|> sub_op)) input

  and mult_expr input =
    (chainl1 unary_expr (mul_op <|> div_op <|> mod_op)) input

  and unary_expr input =
    choice
      [
        (token "!" >> lexeme primary >>= fun s -> return (Not s));
        ( token "-" >> lexeme primary >>= fun x ->
          return (Sub (Const (VInt 0), x)) );
        (token "++" >> lexeme primary >>= fun x -> return (PrefInc x));
        (token "--" >> lexeme primary >>= fun x -> return (PrefDec x));
        (lexeme primary >>= fun x -> token "++" >> return (PostInc x));
        (lexeme primary >>= fun x -> token "--" >> return (PostDec x));
        primary;
      ]
      input

  and primary input =
    ( create_obj <|> create_arr <|> assign <|> field_access <|> arr_access
    <|> method_call <|> this <|> super <|> parens expression <|> atomic )
      input

  and arr_access input =
    ( this <|> parens create_arr <|> super <|> method_call <|> identifier
    >>= fun arr_name ->
      brackets expression >>= fun index ->
      return (ArrayAccess (arr_name, index)) )
      input

  and field_access input =
    let fold_arr_field acc el =
      match el with
      | ArrayAccess (ident, i) -> ArrayAccess (FieldAccess (acc, ident), i)
      | other -> FieldAccess (acc, other)
    in
    let f_parse =
      this <|> super <|> parens create_obj <|> arr_access <|> method_call
      <|> identifier
    in
    ( f_parse >>= fun head ->
      many1 (token "." >> f_parse) => fun tl ->
      List.fold_left fold_arr_field head tl )
      input

  and expr_sep_by_comma input = sep_by expression (token ",") input

  and method_call input =
    ( identifier <|> this <|> super >>= fun m_name ->
      token "(" >> expr_sep_by_comma >>= fun expr_list ->
      token ")" >> return (CallMethod (m_name, expr_list)) )
      input

  and create_obj input =
    ( token "new" >> name >>= fun class_name ->
      token "(" >> expr_sep_by_comma >>= fun expr_list ->
      token ")" >> return (ClassCreate (class_name, expr_list)) )
      input

  and create_arr input =
    ( token "new" >> type_spec >>= fun ts ->
      choice
        [
          ( token "[]" >> token "{" >> sep_by1 expression (token ",")
          >>= fun el_list -> return (ArrayCreateElements (ts, el_list)) );
          ( brackets expression >>= fun size ->
            return (ArrayCreateSized (ts, size)) );
        ] )
      input

  and assign input =
    let a_left = field_access <|> arr_access <|> method_call <|> identifier in
    ( a_left >>= fun left ->
      token "=" >> expression >>= fun right -> return (Assign (left, right)) )
      input
end

module Stmt = struct
  open Expr

  let break_stat = token "break" >> token ";" >> return Break

  let%test _ = apply break_stat "break;" = Some Break

  let continue_stat = token "continue" >> token ";" >> return Continue

  let%test _ = apply continue_stat "continue;" = Some Continue

  let return_stat =
    token "return "
    >> choice
         [
           (expression >>= fun ret -> token ";" >> return (Return (Some ret)));
           token ";" >> return (Return None);
         ]

  let%test _ =
    apply return_stat "return 0;" = Some (Return (Some (Const (VInt 0))))

  let%test _ =
    apply return_stat "return a < b;"
    = Some (Return (Some (Less (Identifier "a", Identifier "b"))))

  let expr_stat =
    expression >>= fun expr -> token ";" >> return (Expression expr)

  let%test _ =
    apply expr_stat "fork();"
    = Some (Expression (CallMethod (Identifier "fork", [])))

  let%test _ =
    apply expr_stat "i++;" = Some (Expression (PostInc (Identifier "i")))

  let rec statement input =
    choice
      [
        var_declaration;
        break_stat;
        continue_stat;
        return_stat;
        if_stat;
        while_stat;
        for_stat;
        throw_stat;
        expr_stat;
        stat_block;
      ]
      input

  and if_stat input =
    ( token "if" >> token "(" >> expression >>= fun cond_expr ->
      token ")" >> statement >>= fun then_stats ->
      choice
        [
          ( token "else" >> statement >>= fun else_stats ->
            return (If (cond_expr, then_stats, Some else_stats)) );
          return (If (cond_expr, then_stats, None));
        ] )
      input

  and stat_block input =
    ( token "{" >> sep_by statement spaces >>= fun block_stats ->
      token "}" >> return (StmtBlock block_stats) )
      input

  and while_stat input =
    ( token "while" >> token "(" >> expression >>= fun cond_expr ->
      token ")" >> statement >>= fun stat -> return (While (cond_expr, stat)) )
      input

  and var_declaration =
    let var_declarator =
      name >>= fun v_name ->
      token "=" >> expression
      >>= (fun value -> return (v_name, Some value))
      <|> return (v_name, None)
    in
    type_spec_array >>= fun type_specifier ->
    sep_by1 var_declarator (token ",") >>= fun dec_pairs ->
    token ";" >> return (VarDec (type_specifier, dec_pairs))

  and for_stat input =
    ( token "for" >> token "("
    >> choice
         [
           (statement >>= fun stat -> return (Some stat));
           token ";" >> return None;
         ]
    >>= fun dec ->
      choice
        [
          (expression >>= fun expr -> token ";" >> return (Some expr));
          token ";" >> return None;
        ]
      >>= fun cond ->
      sep_by expression (token ",") >>= fun after ->
      token ")" >> statement >>= fun body ->
      return (For (dec, cond, after, body)) )
      input

  and throw_stat =
    token "throw" >> expression >>= fun expr -> token ";" >> return (Throw expr)
end

let method_declaration =
  let param =
    Expr.type_spec_array >>= fun type_par ->
    name >>= fun id_par -> return (type_par, id_par)
  in
  many modifier >>= fun modifiers ->
  Expr.type_spec_array >>= fun m_type ->
  name >>= fun m_name ->
  token "(" >> sep_by param (token ",") >>= fun param_list ->
  token ")"
  >> choice
       [
         ( Stmt.stat_block >>= fun st_block ->
           return
             (Method (modifiers, m_type, m_name, param_list, Some st_block)) );
         token ";"
         >> return (Method (modifiers, m_type, m_name, param_list, None));
       ]

let constructor_declaration =
  let param =
    Expr.type_spec_array >>= fun type_par ->
    name >>= fun id_par -> return (type_par, id_par)
  in
  many modifier >>= fun modifiers ->
  name >>= fun c_name ->
  token "(" >> sep_by param (token ",") >>= fun param_list ->
  token ")" >> Stmt.stat_block >>= fun c_block ->
  return (Constructor (modifiers, c_name, param_list, c_block))

let field_declaration =
  let var_declarator =
    name >>= fun name ->
    token "=" >> Expr.expression
    >>= (fun value -> return (name, Some value))
    <|> return (name, None)
  in
  many modifier >>= fun modifiers ->
  Expr.type_spec_array >>= fun type_specifier ->
  sep_by var_declarator (token ",") >>= fun dec_pairs ->
  token ";" >> return (VarField (modifiers, type_specifier, dec_pairs))

let class_elem =
  field_declaration <|> constructor_declaration <|> method_declaration

let class_declaration =
  many modifier >>= fun modifiers ->
  token "class" >> name >>= fun class_name ->
  choice
    [
      (token "extends" >> name >>= fun parent_name -> return (Some parent_name));
      return None;
    ]
  >>= fun extension ->
  token "{" >> sep_by class_elem spaces >>= fun class_elements ->
  token "}" >> return (Class (modifiers, class_name, extension, class_elements))

let parser = many class_declaration
