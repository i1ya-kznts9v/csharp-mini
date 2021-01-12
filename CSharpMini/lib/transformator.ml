open Ast
open Parser
open Pretty_printer
open Format

(*-_-_-_-_-_-_-_-_-_-_- Variables renaming -_-_-_-_-_-_-_-_-_-_-*)
let pp_difference pp_old pp_new pp_format = function
  | element ->
      fprintf pp_format "-- %a\n++ %a\n\n" pp_old element pp_new element

let rec look_up_expression old_name = function
  | Add (left, right)
   |Sub (left, right)
   |Div (left, right)
   |Mod (left, right)
   |Mult (left, right)
   |Or (left, right)
   |And (left, right)
   |Equal (left, right)
   |NotEqual (left, right)
   |Less (left, right)
   |More (left, right)
   |LessOrEqual (left, right)
   |MoreOrEqual (left, right)
   |Assign (left, right)
   |AccessByPoint (left, right)
   |ArrayAccess (left, right) ->
      look_up_expression old_name left || look_up_expression old_name right
  | Identifier name -> old_name = name
  | This | Base | Null | Value _ -> false
  | PrefInc operand
   |PrefDec operand
   |PostInc operand
   |PostDec operand
   |Not operand ->
      look_up_expression old_name operand
  | CallMethod (_, arguments)
   |ClassCreation (_, arguments)
   |ArrayCreationWithElements (_, arguments) ->
      List.exists (fun operand -> look_up_expression old_name operand) arguments
  | ArrayCreationWithSize (_, size) -> look_up_expression old_name size

let rec pp_rn_expession old_name new_name pp_format = function
  | Add (left, right) ->
      fprintf pp_format "%a + %a"
        (pp_rn_expession old_name new_name)
        left
        (pp_rn_expession old_name new_name)
        right
  | Sub (left, right) ->
      fprintf pp_format "%a - %a"
        (pp_rn_expession old_name new_name)
        left
        (pp_rn_expession old_name new_name)
        right
  | Mult (left, right) ->
      if was_add_or_sub left then (
        fprintf pp_format "%a * "
          (pp_rn_expession_parens old_name new_name)
          left ;
        if was_add_or_sub right then
          fprintf pp_format "%a"
            (pp_rn_expession_parens old_name new_name)
            right
        else fprintf pp_format "%a" (pp_rn_expession old_name new_name) right )
      else (
        fprintf pp_format "%a * " (pp_rn_expession old_name new_name) left ;
        if was_add_or_sub right then
          fprintf pp_format "%a"
            (pp_rn_expession_parens old_name new_name)
            right
        else fprintf pp_format "%a" (pp_rn_expession old_name new_name) right )
  | Div (left, right) ->
      if was_add_or_sub left then (
        fprintf pp_format "%a / "
          (pp_rn_expession_parens old_name new_name)
          left ;
        if was_add_or_sub right then
          fprintf pp_format "%a"
            (pp_rn_expession_parens old_name new_name)
            right
        else fprintf pp_format "%a" (pp_rn_expession old_name new_name) right )
      else (
        fprintf pp_format "%a / " (pp_rn_expession old_name new_name) left ;
        if was_add_or_sub right then
          fprintf pp_format "%a"
            (pp_rn_expession_parens old_name new_name)
            right
        else fprintf pp_format "%a" (pp_rn_expession old_name new_name) right )
  | Mod (left, right) ->
      if was_add_or_sub left then (
        fprintf pp_format "%a %% "
          (pp_rn_expession_parens old_name new_name)
          left ;
        if was_add_or_sub right then
          fprintf pp_format "%a"
            (pp_rn_expession_parens old_name new_name)
            right
        else fprintf pp_format "%a" (pp_rn_expession old_name new_name) right )
      else (
        fprintf pp_format "%a %% " (pp_rn_expession old_name new_name) left ;
        if was_add_or_sub right then
          fprintf pp_format "%a"
            (pp_rn_expession_parens old_name new_name)
            right
        else fprintf pp_format "%a" (pp_rn_expession old_name new_name) right )
  | And (left, right) ->
      if was_or left then (
        fprintf pp_format "%a && "
          (pp_rn_expession_parens old_name new_name)
          left ;
        if was_or right then
          fprintf pp_format "%a"
            (pp_rn_expession_parens old_name new_name)
            right
        else fprintf pp_format "%a" (pp_rn_expession old_name new_name) right )
      else (
        fprintf pp_format "%a && " (pp_rn_expession old_name new_name) left ;
        if was_or right then
          fprintf pp_format "%a"
            (pp_rn_expession_parens old_name new_name)
            right
        else fprintf pp_format "%a" (pp_rn_expession old_name new_name) right )
  | Or (left, right) ->
      fprintf pp_format "%a || %a"
        (pp_rn_expession old_name new_name)
        left
        (pp_rn_expession old_name new_name)
        right
  | Not operand ->
      if was_value_or_identifier operand then
        fprintf pp_format "!%a" (pp_rn_expession old_name new_name) operand
      else
        fprintf pp_format "!%a"
          (pp_rn_expession_parens old_name new_name)
          operand
  | Less (left, right) ->
      fprintf pp_format "%a < %a"
        (pp_rn_expession old_name new_name)
        left
        (pp_rn_expession old_name new_name)
        right
  | More (left, right) ->
      fprintf pp_format "%a > %a"
        (pp_rn_expession old_name new_name)
        left
        (pp_rn_expession old_name new_name)
        right
  | LessOrEqual (left, right) ->
      fprintf pp_format "%a <= %a"
        (pp_rn_expession old_name new_name)
        left
        (pp_rn_expession old_name new_name)
        right
  | MoreOrEqual (left, right) ->
      fprintf pp_format "%a >= %a"
        (pp_rn_expession old_name new_name)
        left
        (pp_rn_expession old_name new_name)
        right
  | Equal (left, right) ->
      fprintf pp_format "%a == %a"
        (pp_rn_expession old_name new_name)
        left
        (pp_rn_expession old_name new_name)
        right
  | NotEqual (left, right) ->
      fprintf pp_format "%a != %a"
        (pp_rn_expession old_name new_name)
        left
        (pp_rn_expession old_name new_name)
        right
  | PostInc operand ->
      fprintf pp_format "%a++" (pp_rn_expession old_name new_name) operand
  | PrefInc operand ->
      fprintf pp_format "++%a" (pp_rn_expession old_name new_name) operand
  | PostDec operand ->
      fprintf pp_format "%a--" (pp_rn_expession old_name new_name) operand
  | PrefDec operand ->
      fprintf pp_format "--%a" (pp_rn_expession old_name new_name) operand
  | Null -> fprintf pp_format "null"
  | This -> fprintf pp_format "this"
  | Base -> fprintf pp_format "base"
  | Value value -> fprintf pp_format "%a" pp_value value
  | Identifier name ->
      if name = old_name then fprintf pp_format "%s" new_name
      else fprintf pp_format "%s" name
  | AccessByPoint (left, right) ->
      if was_creation left then
        fprintf pp_format "%a.%a"
          (pp_rn_expession_parens old_name new_name)
          left
          (pp_rn_expession old_name new_name)
          right
      else
        fprintf pp_format "%a.%a"
          (pp_rn_expession old_name new_name)
          left
          (pp_rn_expession old_name new_name)
          right
  | ArrayAccess (array, index) ->
      fprintf pp_format "%a[%a]"
        (pp_rn_expession old_name new_name)
        array
        (pp_rn_expession old_name new_name)
        index
  | ArrayCreationWithSize (array_type, size) ->
      fprintf pp_format "new %a[%a]" pp_type array_type
        (pp_rn_expession old_name new_name)
        size
  | ArrayCreationWithElements (array_type, expression_list) ->
      fprintf pp_format "new %a[] {%a}" pp_type array_type
        (pp_rn_expession_list old_name new_name)
        expression_list
  | ClassCreation (name, expression_list) ->
      fprintf pp_format "new %a(%a)" pp_name name
        (pp_rn_expession_list old_name new_name)
        expression_list
  | CallMethod (m_expr, expr_list) ->
      fprintf pp_format "%a(%a)"
        (pp_rn_expession old_name new_name)
        m_expr
        (pp_rn_expession_list old_name new_name)
        expr_list
  | Assign (left, right) ->
      fprintf pp_format "%a = %a"
        (pp_rn_expession old_name new_name)
        left
        (pp_rn_expession old_name new_name)
        right

and pp_rn_expession_parens old_name new_nane pp_format =
  fprintf pp_format "(%a)" (pp_rn_expession old_name new_nane)

and pp_rn_expession_list old_name new_nane pp_format =
  pp_print_list ~pp_sep:pp_sep_comma
    (pp_rn_expession old_name new_nane)
    pp_format

let look_up_name_expression_pair old_name =
  List.exists (function Name name, expression_opt ->
      ( match expression_opt with
      | Some expression ->
          name = old_name || look_up_expression old_name expression
      | None -> name = old_name ))

let pp_rn_name_expression_pair old_name new_name pp_format = function
  | Name name, expression_opt -> (
    match expression_opt with
    | Some expression ->
        if name = old_name then
          fprintf pp_format "%s = %a" new_name
            (pp_rn_expession old_name new_name)
            expression
        else
          fprintf pp_format "%s = %a" name
            (pp_rn_expession old_name new_name)
            expression
    | None ->
        if name = old_name then fprintf pp_format "%s" new_name
        else fprintf pp_format "%s" name )

let pp_rn_name_expression_pair_list old_name new_name pp_format =
  pp_print_list ~pp_sep:pp_sep_comma
    (pp_rn_name_expression_pair old_name new_name)
    pp_format

let pp_field pp_format = function
  | modifier_list, Field (typeq, name_expression_pair) ->
      fprintf pp_format "%a %a %a;" pp_modifier_list modifier_list pp_type typeq
        pp_pairs_decl_list name_expression_pair
  | _ -> ()

let pp_rn_field old_name new_name pp_format = function
  | modifier_list, Field (typeq, name_expression_pair) ->
      fprintf pp_format "%a %a %a;" pp_modifier_list modifier_list pp_type typeq
        (pp_rn_name_expression_pair_list old_name new_name)
        name_expression_pair
  | _ -> ()

let rec pp_rn_statement old_name new_name pp_format = function
  | If (condition, then_statement, else_statement_opt) -> (
      let pp_condition pp_e pp_format be = fprintf pp_format "if (%a)" pp_e be in
      if look_up_expression old_name condition then
        pp_difference
          (pp_condition pp_expession)
          (pp_condition (pp_rn_expession old_name new_name))
          pp_format condition
      else () ;
      pp_rn_statement old_name new_name pp_format then_statement ;
      match else_statement_opt with
      | Some else_statement ->
          pp_rn_statement old_name new_name pp_format else_statement
      | None -> () )
  | While (condition, body) ->
      let pp_condition pp_e pp_format be =
        fprintf pp_format "while (%a)" pp_e be in
      if look_up_expression old_name condition then
        pp_difference
          (pp_condition pp_expession)
          (pp_condition (pp_rn_expession old_name new_name))
          pp_format condition
      else () ;
      pp_rn_statement old_name new_name pp_format body
  | Return expression_opt -> (
    match expression_opt with
    | None -> ()
    | Some expresion ->
        let pp_return pp_e pp_format e = fprintf pp_format "return %a;" pp_e e in
        if look_up_expression old_name expresion then
          pp_difference (pp_return pp_expession)
            (pp_return (pp_rn_expession old_name new_name))
            pp_format expresion
        else () )
  | For (decl_statement_opt, condition_opt, after_expression_list, body) ->
      let pp_rn_initialization old_name new_name pp_format = function
        | VariableDecl (modifier_opt, variable_type, name_expression_pair_list)
          -> (
          match modifier_opt with
          | Some modifier ->
              fprintf pp_format "%a %a %a;" pp_modifier modifier pp_type
                variable_type
                (pp_rn_name_expression_pair_list old_name new_name)
                name_expression_pair_list
          | None ->
              fprintf pp_format "%a %a;" pp_type variable_type
                (pp_rn_name_expression_pair_list old_name new_name)
                name_expression_pair_list )
        | _ -> () in
      ( match (decl_statement_opt, condition_opt) with
      | Some decl_statement, Some condition ->
          let pp_for pp_s pp_e pp_aft pp_format = function
            | de, be, aft ->
                fprintf pp_format "for (%a; %a; %a)" pp_s de pp_e be pp_aft aft
          in
          ( match decl_statement with
          | VariableDecl (_, _, name_expression_pair_list) ->
              name_expression_pair_list
          | _ -> [] )
          |> fun pair_list ->
          if
            look_up_expression old_name condition
            || look_up_name_expression_pair old_name pair_list
            || List.exists (look_up_expression old_name) after_expression_list
          then
            pp_difference
              (pp_for pp_statement pp_expession pp_expession_list)
              (pp_for
                 (pp_rn_initialization old_name new_name)
                 (pp_rn_expession old_name new_name)
                 (pp_rn_expession_list old_name new_name))
              pp_format
              (decl_statement, condition, after_expression_list)
      | None, Some condition ->
          let pp_for pp_e pp_aft pp_format = function
            | be, aft -> fprintf pp_format "for ( ; %a; %a)" pp_e be pp_aft aft
          in
          if
            look_up_expression old_name condition
            || List.exists (look_up_expression old_name) after_expression_list
          then
            pp_difference
              (pp_for pp_expession pp_expession_list)
              (pp_for
                 (pp_rn_expession old_name new_name)
                 (pp_rn_expession_list old_name new_name))
              pp_format
              (condition, after_expression_list)
      | Some decl_statement, None ->
          let pp_for pp_st pp_aft pp_format = function
            | de, aft -> fprintf pp_format "for (%a; ; %a)" pp_st de pp_aft aft
          in
          ( match decl_statement with
          | VariableDecl (_, _, name_expression_pair_list) ->
              name_expression_pair_list
          | _ -> [] )
          |> fun pair_list ->
          if
            look_up_name_expression_pair old_name pair_list
            || List.exists (look_up_expression old_name) after_expression_list
          then
            pp_difference
              (pp_for pp_statement pp_expession_list)
              (pp_for
                 (pp_rn_initialization old_name new_name)
                 (pp_rn_expession_list old_name new_name))
              pp_format
              (decl_statement, after_expression_list)
      | None, None ->
          let pp_for pp_aft pp_format = function
            | aft -> fprintf pp_format "for ( ; ; %a)" pp_aft aft in
          if List.exists (look_up_expression old_name) after_expression_list
          then
            pp_difference (pp_for pp_expession_list)
              (pp_for (pp_rn_expession_list old_name new_name))
              pp_format after_expression_list ) ;
      pp_rn_statement old_name new_name pp_format body
  | VariableDecl (modifier_o, variable_type, name_expresion_pair_list) -> (
      if look_up_name_expression_pair old_name name_expresion_pair_list then
        match modifier_o with
        | Some modifier ->
            let pp_varible_decl pp_e pp_format = function
              | md, vt, pl ->
                  fprintf pp_format "%a %a %a;" pp_modifier md pp_type vt pp_e
                    pl in
            pp_difference
              (pp_varible_decl pp_pairs_decl_list)
              (pp_varible_decl
                 (pp_rn_name_expression_pair_list old_name new_name))
              pp_format
              (modifier, variable_type, name_expresion_pair_list)
        | None ->
            let pp_varible_decl pp_e pp_format = function
              | vt, pl -> fprintf pp_format "%a %a;" pp_type vt pp_e pl in
            pp_difference
              (pp_varible_decl pp_pairs_decl_list)
              (pp_varible_decl
                 (pp_rn_name_expression_pair_list old_name new_name))
              pp_format
              (variable_type, name_expresion_pair_list) )
  | Expression expression ->
      if look_up_expression old_name expression then
        let pp_expr pp_e pp_format = function
          | e -> fprintf pp_format "%a;" pp_e e in
        pp_difference (pp_expr pp_expession)
          (pp_expr (pp_rn_expession old_name new_name))
          pp_format expression
  | StatementBlock statement_block ->
      List.iter (pp_rn_statement old_name new_name pp_format) statement_block
  | Break | Continue -> ()

let look_up_type_name_pair old_name =
  let look_up_name old_name = function _, Name name -> name = old_name in
  List.exists (look_up_name old_name)

let pp_rn_type_name_pair_list old_name new_name pp_format =
  let pp_type_name_pair old_name new_name pp_format = function
    | t, Name n ->
        if n = old_name then fprintf pp_format "%a %s" pp_type t new_name
        else fprintf pp_format "%a %s" pp_type t n in
  pp_print_list ~pp_sep:pp_sep_comma
    (pp_type_name_pair old_name new_name)
    pp_format

let pp_rn_class_element old_name new_name pp_format element =
  match element with
  | _, Field (_, name_expression_pair_list) ->
      if look_up_name_expression_pair old_name name_expression_pair_list then
        pp_difference pp_field (pp_rn_field old_name new_name) pp_format element
  | ml, Method (method_type, method_name, arguments, body_opt) -> (
      ( if look_up_type_name_pair old_name arguments then
        let pp_method pp_args pp_format = function
          | arg ->
              fprintf pp_format "%a %a %a(%a)" pp_modifier_list ml pp_type
                method_type pp_name method_name pp_args arg in
        pp_difference
          (pp_method pp_type_name_pairs_list)
          (pp_method (pp_rn_type_name_pair_list old_name new_name))
          pp_format arguments ) ;
      match body_opt with
      | Some body -> pp_rn_statement old_name new_name pp_format body
      | None -> () )
  | ml, Constructor (constructor_name, arguments, call_constructor, body) ->
      ( if look_up_type_name_pair old_name arguments then
        let pp_constructor pp_args pp_expr ppf = function
          | arg -> (
            match call_constructor with
            | None ->
                fprintf ppf "%a %a(%a)" pp_modifier_list ml pp_name
                  constructor_name pp_args arg
            | Some expr ->
                fprintf ppf "%a %a(%a) : %a" pp_modifier_list ml pp_name
                  constructor_name pp_args arg pp_expr expr ) in
        pp_difference
          (pp_constructor pp_type_name_pairs_list pp_expession)
          (pp_constructor
             (pp_rn_type_name_pair_list old_name new_name)
             (pp_rn_expession old_name new_name))
          pp_format arguments ) ;
      pp_rn_statement old_name new_name pp_format body

let pp_rn_class old_name new_name pp_format = function
  | Class (_, _, _, modifier_element_pair_list) ->
      List.iter
        (pp_rn_class_element old_name new_name pp_format)
        modifier_element_pair_list

let pp_rn_class_list old_name new_name pp_format =
  List.iter (pp_rn_class old_name new_name pp_format)

let rename_transformation program old_name new_name pp_format =
  try
    let classes =
      match apply parser program with
      | None | Some [] -> raise (Invalid_argument "Syntax error or empty file")
      | Some result -> result in
    pp_rn_class_list old_name new_name pp_format classes
  with Invalid_argument message -> fprintf pp_format "%s" message
