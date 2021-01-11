open Ast
open Opal

let apply_parser parser input = parse parser (LazyStream.of_string input)

let reserved =
  [ "true"; "false"; "if"; "else"; "while"; "public"; "static"; "const"
  ; "override"; "try"; "catch"; "finally"; "when"; "void"; "string"; "char"
  ; "Console"; "namespace"; "using"; "int"; "bool"; "for"; "null"; "new"
  ; "return"; "break"; "continue"; "class" ]

let const = token "const" >> return Const
let parens = between (token "(") (token ")")
let braces = between (token "{") (token "}")

let get_modifier_list =
  many
    (choice
       [ token "public" >> return Public; token "static" >> return Static
       ; token "const" >> return Const; token "override" >> return Override ])

let digits = spaces >> many1 digit => implode
let convert_to_int = digits => int_of_string

module Expr = struct
  open Ast

  let add_op = token "+" >> return (fun x y -> Add (x, y))
  let sub_op = token "-" >> return (fun x y -> Sub (x, y))
  let mul_op = token "*" >> return (fun x y -> Mul (x, y))
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
  let null = token "null" >> return Null

  let ident_obj =
    spaces >> letter <~> many alpha_num => implode
    >>= function x when List.mem x reserved -> mzero | x -> return x

  let get_variable = ident_obj => fun x -> IdentVar x

  let parse_string =
    let string_of_chars chars =
      let buf = Buffer.create 16 in
      List.iter (Buffer.add_char buf) chars ;
      Buffer.contents buf in
    token "\""
    >> many (satisfy (fun x -> x <> '\"'))
    >>= fun list ->
    token "\"" >> return (ConstExpr (VString (string_of_chars list)))

  let atomic =
    get_variable
    <|> (convert_to_int >>= fun n -> return (ConstExpr (VInt n)))
    <|> parse_string
    <|> (token "false" >> return (ConstExpr (VBool false)))
    <|> (token "true" >> return (ConstExpr (VBool true)))
    <|> null

  let define_type =
    choice
      [ token "int" >> return Int; token "string" >> return String
      ; token "void" >> return Void; token "bool" >> return Bool
      ; (ident_obj >>= fun class_name -> return (CsClass class_name)) ]

  let rec expr input = num_expr input
  and num_expr input = (chainl1 and_expr or_op) input
  and and_expr input = (chainl1 comp_expr and_op) input

  and comp_expr input =
    (chainl1 add_expr (le_op <|> me_op <|> l_op <|> m_op <|> eq_op <|> neq_op))
      input

  and add_expr input = (chainl1 mul_expr (add_op <|> sub_op)) input
  and mul_expr input = (chainl1 unar_expr (mul_op <|> div_op <|> mod_op)) input

  and unar_expr input =
    choice
      [ (token "!" >> lexeme primar_expr >>= fun x -> return (Not x))
      ; ( token "-" >> lexeme primar_expr
        >>= fun x -> return (Sub (ConstExpr (VInt 0), x)) )
      ; (token "++" >> lexeme primar_expr >>= fun x -> return (PrefInc x))
      ; (token "--" >> lexeme primar_expr >>= fun x -> return (PrefDec x))
      ; (lexeme primar_expr >>= fun x -> token "++" >> return (PostInc x))
      ; (lexeme primar_expr >>= fun x -> token "--" >> return (PostDec x))
      ; primar_expr ]
      input

  and primar_expr input =
    ( init_instance <|> assign <|> field_access <|> call_method <|> parens expr
    <|> atomic )
      input

  and separate_comma input = sep_by expr (token ",") input

  and call_method input =
    ( ident_obj
    >>= fun name ->
    token "(" >> separate_comma
    >>= fun args_list -> token ")" >> return (CallMethod (name, args_list)) )
      input

  and init_instance input =
    ( token "new" >> ident_obj
    >>= fun name ->
    token "(" >> separate_comma
    >>= fun args_list -> token ")" >> return (ClassCreate (name, args_list)) )
      input

  and field_access input =
    let helper = parens init_instance <|> call_method <|> get_variable in
    ( helper
    >>= fun head ->
    many1 (token "." >> helper)
    => fun tl -> List.fold_left (fun head tl -> Access (head, tl)) head tl )
      input

  and assign input =
    let parse_left = field_access <|> call_method <|> get_variable in
    ( parse_left
    >>= fun left ->
    token "=" >> expr >>= fun right -> return (Assign (left, right)) )
      input
end

module Stat = struct
  open Expr

  let rec parse_statement input =
    choice
      [ continue; break; parse_expr; return_stat; if_stat; while_stat; throw
      ; var_declare; for_stat; stat_block; print_func; try_stat ]
      input

  and if_stat input =
    ( token "if" >> parens expr
    >>= fun condition ->
    parse_statement
    >>= fun then_stat ->
    choice
      [ ( token "else" >> parse_statement
        >>= fun else_stat -> return (If (condition, then_stat, Some else_stat))
        ); return (If (condition, then_stat, None)) ] )
      input

  and while_stat input =
    ( token "while" >> parens expr
    >>= fun condition ->
    parse_statement >>= fun stat -> return (While (condition, stat)) )
      input

  and var_declare input =
    let helper =
      ident_obj
      >>= fun var_name ->
      token "=" >> expr
      >>= (fun var_value -> return (var_name, Some var_value))
      <|> return (var_name, None) in
    choice
      [ ( const
        >>= fun modif ->
        define_type
        >>= fun var_type ->
        sep_by1 helper (token ",")
        >>= fun var_pair ->
        token ";" >> return (VarDeclare (Some modif, var_type, var_pair)) )
      ; ( define_type
        >>= fun var_type ->
        sep_by1 helper (token ",")
        >>= fun var_pair ->
        token ";" >> return (VarDeclare (None, var_type, var_pair)) ) ]
      input

  and stat_block input =
    ( braces (sep_by parse_statement spaces)
    >>= fun stats -> return (StatementBlock stats) )
      input

  and for_stat input =
    ( token "for" >> token "("
    >> choice
         [ (var_declare >>= fun stat -> return (Some stat))
         ; token ";" >> return None ]
    >>= fun declare ->
    choice
      [ (expr >>= fun expr -> token ";" >> return (Some expr))
      ; token ";" >> return None ]
    >>= fun condition ->
    sep_by expr (token ",")
    >>= fun after ->
    token ")" >> parse_statement
    >>= fun body -> return (For (declare, condition, after, body)) )
      input

  and print_func input =
    ( token "Console.WriteLine(" >> expr
    >>= fun print_expression -> token ");" >> return (Print print_expression) )
      input

  and throw input =
    ( token "throw" >> expr
    >>= fun throw_expr -> token ";" >> return (Throw throw_expr) )
      input

  and parse_expr input =
    (expr >>= fun express -> token ";" >> return (Expression express)) input

  and return_stat input =
    ( token "return"
    >> choice
         [ ( skip_many1 space >> expr
           >>= fun result -> token ";" >> return (Return (Some result)) )
         ; token ";" >> return (Return None) ] )
      input

  and continue input = (token "continue" >> token ";" >> return Continue) input
  and break input = (token "break" >> token ";" >> return Break) input

  and try_stat input =
    let filter =
      token "when" >> token "(" >> Expr.expr
      >>= (fun filter -> token ")" >> return (Some filter))
      <|> return None in
    let catch =
      token "catch"
      (*here are all 6 possible catch cases:
        1 - catch {}
        2 - catch when (...) {}
        3 - catch (Exception) {}
        4 - catch (Exception) when (...) {}
        5 - catch (Exception ex) {}
        6 - catch (Exception ex) when (..)
        all cases have been tested in tests.ml*)
      >> choice
           [ ( token "(" >> Expr.define_type
             >>= fun excep_type ->
             choice
               [ (Expr.get_variable >>= fun name -> return (Some name))
               ; return None ]
             >>= fun var_name ->
             token ")" >> filter
             >>= fun filter ->
             parse_statement
             >>= fun catch_block ->
             return (Some (excep_type, var_name), filter, catch_block) )
           ; ( filter
             >>= fun filter ->
             parse_statement
             >>= fun catch_block -> return (None, filter, catch_block) ) ] in
    ( token "try" >> parse_statement
    >>= fun try_stat ->
    many catch
    >>= fun catch_list ->
    match catch_list with
    | [] ->
        (*because if there was no catch, then there must be finally, according to the C # documentation*)
        token "finally" >> parse_statement
        >>= fun finally_block ->
        return (Try (try_stat, catch_list, Some finally_block))
    | _ ->
        choice
          [ ( token "finally" >> parse_statement
            >>= fun finally_block ->
            return (Try (try_stat, catch_list, Some finally_block)) )
          ; return (Try (try_stat, catch_list, None)) ] )
      input
end

let get_params =
  Expr.define_type
  >>= fun _type -> Expr.ident_obj >>= fun name -> return (_type, name)

let field =
  let helper =
    Expr.ident_obj
    >>= fun name ->
    token "=" >> Expr.expr
    >>= (fun value -> return (name, Some value))
    <|> return (name, None) in
  Expr.define_type
  >>= fun f_type ->
  sep_by helper (token ",")
  >>= fun var_list -> token ";" >> return (VariableField (f_type, var_list))

let class_method =
  Expr.define_type
  >>= fun method_type ->
  Expr.ident_obj
  >>= fun method_name ->
  token "("
  >> sep_by get_params (token ",")
  >>= fun params_list ->
  token ")" >> Stat.stat_block
  >>= fun stat_block ->
  return (Method (method_type, method_name, params_list, stat_block))

let constructor =
  Expr.ident_obj
  >>= fun name ->
  token "("
  >> sep_by get_params (token ",")
  >>= fun params_list ->
  token ")" >> Stat.stat_block
  >>= fun stat_block -> return (Constructor (name, params_list, stat_block))

let class_elements =
  get_modifier_list
  >>= fun modifiers ->
  field <|> class_method <|> constructor
  >>= fun class_elem -> return (modifiers, class_elem)

let parse_class =
  get_modifier_list
  >>= fun modifiers ->
  token "class" >> Expr.ident_obj
  >>= fun name ->
  choice
    [ (token ":" >> Expr.ident_obj >>= fun parent -> return (Some parent))
    ; return None ]
  >>= fun _parent ->
  token "{"
  >> sep_by class_elements spaces
  >>= fun class_elements ->
  token "}" >> return (Class (modifiers, name, _parent, class_elements))

let parser = many parse_class
