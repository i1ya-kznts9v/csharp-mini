open Format
open Ast

let pp_space ppf () = fprintf ppf " "
let pp_comma ppf () = fprintf ppf ", "
let pp_end_l ppf () = fprintf ppf "@;<0 0>"

(*эквивалент print_break 0 0*)
let pp_two_end_l ppf () = fprintf ppf "@;<0 0>@;<0 0>"

let pp_data_type ppf = function
  | Int -> fprintf ppf "int"
  | Bool -> fprintf ppf "bool"
  | Void -> fprintf ppf "void"
  | String -> fprintf ppf "string"
  | CsClass cl_name -> fprintf ppf "%s" cl_name

let pp_value ppf = function
  | VInt i -> fprintf ppf "%d" i
  | VBool b -> fprintf ppf "%b" b
  | VString s -> fprintf ppf "\"%s\"" s
  | _ -> ()

let pp_modifier ppf = function
  | Public -> fprintf ppf "%s" "public"
  | Static -> fprintf ppf "%s" "static"
  | Const -> fprintf ppf "%s" "const"
  | Override -> fprintf ppf "%s" "override"

(*с использованием необязательного аргумента*)
let pp_modifier_list ppf = pp_print_list ~pp_sep:pp_space pp_modifier ppf
let check_add_or_sub = function Add (_, _) | Sub (_, _) -> true | _ -> false
let check_or = function Or (_, _) -> true | _ -> false
let check_const_or_id = function ConstExpr _ | IdentVar _ -> true | _ -> false
let check_create = function ClassCreate (_, _) -> true | _ -> false
let pp_name ppf name = fprintf ppf "%s" name

let rec pp_expression ppf = function
  | Add (left, right) ->
      fprintf ppf "%a + %a" pp_expression left pp_expression right
  | Sub (left, right) ->
      fprintf ppf "%a - %a" pp_expression left pp_expression right
      (*В умножении и делении мы смотрим на левый и правые операнды, чтобы правильно поставить скобки,
        если там внутри было или сложение или вычитание*)
  | Mul (left, right) ->
      if check_add_or_sub left then (
        fprintf ppf "%a * " pp_expr_parens left ;
        if check_add_or_sub right then fprintf ppf "%a" pp_expr_parens right
        else fprintf ppf "%a" pp_expression right )
      else (
        fprintf ppf "%a * " pp_expression left ;
        if check_add_or_sub right then fprintf ppf "%a" pp_expr_parens right
        else fprintf ppf "%a" pp_expression right )
  | Div (left, right) ->
      if check_add_or_sub left then (
        fprintf ppf "%a / " pp_expr_parens left ;
        if check_add_or_sub right then fprintf ppf "%a" pp_expr_parens right
        else fprintf ppf "%a" pp_expression right )
      else (
        fprintf ppf "%a / " pp_expression left ;
        if check_add_or_sub right then fprintf ppf "%a" pp_expr_parens right
        else fprintf ppf "%a" pp_expression right )
  | Mod (left, right) ->
      if check_add_or_sub left then (
        fprintf ppf "%a %% " pp_expr_parens left ;
        if check_add_or_sub right then fprintf ppf "%a" pp_expr_parens right
        else fprintf ppf "%a" pp_expression right )
      else (
        fprintf ppf "%a %% " pp_expression left ;
        if check_add_or_sub right then fprintf ppf "%a" pp_expr_parens right
        else fprintf ppf "%a" pp_expression right )
  | And (left, right) ->
      (*В логическом И мы смотрим на левый и правые операнды, чтобы правильно поставить скобки,
              если там внутри было или*)
      if check_or left then (
        fprintf ppf "%a && " pp_expr_parens left ;
        if check_or right then fprintf ppf "%a" pp_expr_parens right
        else fprintf ppf "%a" pp_expression right )
      else (
        fprintf ppf "%a && " pp_expression left ;
        if check_or right then fprintf ppf "%a" pp_expr_parens right
        else fprintf ppf "%a" pp_expression right )
  | Or (left, right) ->
      fprintf ppf "%a || %a" pp_expression left pp_expression right
  | Not ex ->
      if check_const_or_id ex then fprintf ppf "!%a" pp_expression ex
      else fprintf ppf "!%a" pp_expr_parens ex
  | Less (left, right) ->
      fprintf ppf "%a < %a" pp_expression left pp_expression right
  | More (left, right) ->
      fprintf ppf "%a > %a" pp_expression left pp_expression right
  | LessOrEqual (left, right) ->
      fprintf ppf "%a <= %a" pp_expression left pp_expression right
  | MoreOrEqual (left, right) ->
      fprintf ppf "%a >= %a" pp_expression left pp_expression right
  | Equal (left, right) ->
      fprintf ppf "%a == %a" pp_expression left pp_expression right
  | NotEqual (left, right) ->
      fprintf ppf "%a != %a" pp_expression left pp_expression right
  | PostInc exp -> fprintf ppf "%a++" pp_expression exp
  | PrefInc exp -> fprintf ppf "++%a" pp_expression exp
  | PostDec exp -> fprintf ppf "%a--" pp_expression exp
  | PrefDec exp -> fprintf ppf "--%a" pp_expression exp
  | Null -> fprintf ppf "null"
  | ConstExpr value -> fprintf ppf "%a" pp_value value
  | IdentVar var -> fprintf ppf "%s" var
  | Access (left, right) ->
      if check_create left then
        fprintf ppf "%a.%a" pp_expr_parens left pp_expression right
      else fprintf ppf "%a.%a" pp_expression left pp_expression right
  | ClassCreate (name, expr_list) ->
      fprintf ppf "new %a(%a)" pp_name name pp_expression_list expr_list
  | CallMethod (m_name, expr_list) ->
      fprintf ppf "%a(%a)" pp_name m_name pp_expression_list expr_list
  | Assign (left, right) ->
      fprintf ppf "%a = %a" pp_expression left pp_expression right

(*Обертка в скобки*)
and pp_expr_parens ppf = fprintf ppf "(%a)" pp_expression
and pp_expression_list ppf = pp_print_list ~pp_sep:pp_comma pp_expression ppf

let pp_declaration_list ppf =
  let pp_pair ppf = function
    | name, None -> fprintf ppf "%s" name
    | name, Some expr -> fprintf ppf "%s = %a" name pp_expression expr in
  pp_print_list ~pp_sep:pp_comma pp_pair ppf

let rec pp_statement ppf = function
  | Expression expr -> fprintf ppf "%a;" pp_expression expr
  | If (condit, then_stat, else_stat_o) -> (
    match else_stat_o with
    | Some else_stat -> (
      match then_stat with
      | StatementBlock _ ->
          fprintf ppf "if (%a) %a@;<0 0>else %a" pp_expression condit
            pp_statement then_stat pp_statement else_stat
      | _ ->
          fprintf ppf "if (%a) %a else %a" pp_expression condit pp_statement
            then_stat pp_statement else_stat )
    | None ->
        fprintf ppf "if (%a) %a" pp_expression condit pp_statement then_stat )
  | While (condit, body) ->
      fprintf ppf "while (%a) %a" pp_expression condit pp_statement body
  | For (dec_stat_o, condit_o, after_exp_list, body) ->
      ( match dec_stat_o with
      | Some dec_stat -> fprintf ppf "for (%a" pp_statement dec_stat
      | None -> fprintf ppf "for (;" ) ;
      ( match condit_o with
      | Some condit -> fprintf ppf " %a;" pp_expression condit
      | None -> fprintf ppf ";" ) ;
      fprintf ppf " %a) " pp_expression_list after_exp_list ;
      fprintf ppf "%a" pp_statement body
  | Break -> fprintf ppf "break;"
  | Continue -> fprintf ppf "continue;"
  | Return expression_o -> (
    match expression_o with
    | Some expr -> fprintf ppf "return %a;" pp_expression expr
    | None -> fprintf ppf "return;" )
  | VarDeclare (modifier_o, var_type, decl_list) ->
      ( match modifier_o with
      | Some modif -> fprintf ppf "%a " pp_modifier modif
      | None -> () ) ;
      fprintf ppf "%a " pp_data_type var_type ;
      fprintf ppf "%a;" pp_declaration_list decl_list
  | StatementBlock statement_list ->
      fprintf ppf "@;<0 0>@[<v 2>{@;<0 0>%a@;<0 -2>}@]" pp_stat_list
        statement_list
  | Throw expr -> fprintf ppf "throw %a;" pp_expression expr
  | Print expr -> fprintf ppf "Console.WriteLine(%a);" pp_expression expr
  | Try (try_body, catch_list, finally_o) -> (
      fprintf ppf "try %a@;<0 0>" pp_statement try_body ;
      fprintf ppf "%a" pp_catch_list catch_list ;
      match finally_o with
      | Some finally_stat -> fprintf ppf "finally %a" pp_statement finally_stat
      | None -> () )

and pp_catch_list ppf =
  let pp_catch ppf = function
    | None, None, catch_stat ->
        fprintf ppf "catch" ;
        fprintf ppf "%a" pp_statement catch_stat
    | None, Some condit, catch_stat ->
        fprintf ppf "catch when (%a)" pp_expression condit ;
        fprintf ppf "%a" pp_statement catch_stat
    | Some (CsClass cl_name, None), None, catch_stat ->
        fprintf ppf "catch (%a)" pp_name cl_name ;
        fprintf ppf "%a" pp_statement catch_stat
    | Some (CsClass cl_name, None), Some condit, catch_stat ->
        fprintf ppf "catch (%a) when (%a)" pp_name cl_name pp_expression condit ;
        fprintf ppf "%a" pp_statement catch_stat
    | Some (CsClass cl_name, Some (IdentVar th_ex_name)), None, catch_stat ->
        fprintf ppf "catch (%a %a)" pp_name cl_name pp_name th_ex_name ;
        fprintf ppf "%a" pp_statement catch_stat
    | ( Some (CsClass cl_name, Some (IdentVar th_ex_name))
      , Some condit
      , catch_stat ) ->
        fprintf ppf "catch (%a %a) when (%a)" pp_name cl_name pp_name th_ex_name
          pp_expression condit ;
        fprintf ppf "%a" pp_statement catch_stat
    | _ -> fprintf ppf "" in
  pp_print_list ~pp_sep:pp_end_l pp_catch ppf

and pp_stat_list ppf = pp_print_list ~pp_sep:pp_end_l pp_statement ppf

let pp_type_name_pairs_list ppf =
  let pp_type_name_pair ppf = function
    | v_type, name -> fprintf ppf "%a %a" pp_data_type v_type pp_name name in
  pp_print_list ~pp_sep:pp_comma pp_type_name_pair ppf

let pp_field ppf = function
  | mod_list, Method (meth_type, meth_name, args, body) ->
      fprintf ppf "%a" pp_modifier_list mod_list ;
      pp_space ppf () ;
      fprintf ppf "%a %a(%a) %a" pp_data_type meth_type pp_name meth_name
        pp_type_name_pairs_list args pp_statement body
  | mod_list, Constructor (cons_name, args, body) ->
      fprintf ppf "%a %a(%a) %a" pp_modifier_list mod_list pp_name cons_name
        pp_type_name_pairs_list args pp_statement body
  | mod_list, VariableField (var_type, variables) ->
      fprintf ppf "%a %a %a;" pp_modifier_list mod_list pp_data_type var_type
        pp_declaration_list variables

let pp_fields ppf = pp_print_list ~pp_sep:pp_two_end_l pp_field ppf

let pp_cs_class ppf = function
  | Class (mod_list, class_name, parent_name_o, fields) ->
      fprintf ppf "%a" pp_modifier_list mod_list ;
      (match mod_list with [] -> fprintf ppf "" | _ -> fprintf ppf " ") ;
      fprintf ppf "class %a" pp_name class_name ;
      ( match parent_name_o with
      | Some parent_name -> fprintf ppf " : %a" pp_name parent_name
      | None -> fprintf ppf "" ) ;
      fprintf ppf "@;<0 0>@[<v 2>{@;<0 0>%a@;<0 -2>}@]" pp_fields fields

let pp_class_list ppf =
  let pp_class_dec ppf = pp_print_list ~pp_sep:pp_two_end_l pp_cs_class ppf in
  fprintf ppf "@[<v 0>%a@]@;<0 0>@;<0 0>" pp_class_dec

let print_pretty = pp_class_list std_formatter
