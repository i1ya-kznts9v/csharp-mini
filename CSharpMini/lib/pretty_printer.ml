open Ast
open Format

let pp_sep_space pp_format () = fprintf pp_format " "
let pp_sep_comma pp_format () = fprintf pp_format ", "
let pp_sep_endline pp_format () = fprintf pp_format "@;<0 0>"
let pp_sep_two_endline pp_format () = fprintf pp_format "@;<0 0>@;<0 0>"

let pp_value pp_format = function
  | VBool b -> fprintf pp_format "%b" b
  | VInt i -> fprintf pp_format "%d" i
  | VString s -> fprintf pp_format "\"%s\"" s
  | _ -> ()

let rec pp_type pp_format = function
  | TInt -> fprintf pp_format "int"
  | TBool -> fprintf pp_format "bool"
  | TVoid -> fprintf pp_format "void"
  | TString -> fprintf pp_format "string"
  | TClass s -> fprintf pp_format "%s" s
  | TArray t -> fprintf pp_format "%a[]" pp_type t

let pp_modifier pp_format = function
  | Public -> fprintf pp_format "%s" "public"
  | Static -> fprintf pp_format "%s" "static"
  | Const -> fprintf pp_format "%s" "const"
  | Abstract -> fprintf pp_format "%s" "abstract"
  | Sealed -> fprintf pp_format "%s" "sealed"
  | Override -> fprintf pp_format "%s" "override"
  | Virtual -> fprintf pp_format "%s" "virtual"

let pp_modifier_list pp_format =
  pp_print_list ~pp_sep:pp_sep_space pp_modifier pp_format

let was_add_or_sub = function Add (_, _) | Sub (_, _) -> true | _ -> false
let was_or = function Or (_, _) -> true | _ -> false

let was_value_or_identifier = function
  | Value _ | Identifier _ -> true
  | _ -> false

let was_creation = function ClassCreation (_, _) -> true | _ -> false
let pp_name pp_format = function Name n -> fprintf pp_format "%s" n

let rec pp_expession pp_format = function
  | Add (left, right) ->
      fprintf pp_format "%a + %a" pp_expession left pp_expession right
  | Sub (left, right) ->
      fprintf pp_format "%a - %a" pp_expession left pp_expession right
  | Mult (left, right) ->
      if was_add_or_sub left then (
        fprintf pp_format "%a * " pp_expession_parens left ;
        if was_add_or_sub right then
          fprintf pp_format "%a" pp_expession_parens right
        else fprintf pp_format "%a" pp_expession right )
      else (
        fprintf pp_format "%a * " pp_expession left ;
        if was_add_or_sub right then
          fprintf pp_format "%a" pp_expession_parens right
        else fprintf pp_format "%a" pp_expession right )
  | Div (left, right) ->
      if was_add_or_sub left then (
        fprintf pp_format "%a / " pp_expession_parens left ;
        if was_add_or_sub right then
          fprintf pp_format "%a" pp_expession_parens right
        else fprintf pp_format "%a" pp_expession right )
      else (
        fprintf pp_format "%a / " pp_expession left ;
        if was_add_or_sub right then
          fprintf pp_format "%a" pp_expession_parens right
        else fprintf pp_format "%a" pp_expession right )
  | Mod (left, right) ->
      if was_add_or_sub left then (
        fprintf pp_format "%a %% " pp_expession_parens left ;
        if was_add_or_sub right then
          fprintf pp_format "%a" pp_expession_parens right
        else fprintf pp_format "%a" pp_expession right )
      else (
        fprintf pp_format "%a %% " pp_expession left ;
        if was_add_or_sub right then
          fprintf pp_format "%a" pp_expession_parens right
        else fprintf pp_format "%a" pp_expession right )
  | And (left, right) ->
      if was_or left then (
        fprintf pp_format "%a && " pp_expession_parens left ;
        if was_or right then fprintf pp_format "%a" pp_expession_parens right
        else fprintf pp_format "%a" pp_expession right )
      else (
        fprintf pp_format "%a && " pp_expession left ;
        if was_or right then fprintf pp_format "%a" pp_expession_parens right
        else fprintf pp_format "%a" pp_expession right )
  | Or (left, right) ->
      fprintf pp_format "%a || %a" pp_expession left pp_expession right
  | Not operand ->
      if was_value_or_identifier operand then
        fprintf pp_format "!%a" pp_expession operand
      else fprintf pp_format "!%a" pp_expession_parens operand
  | Less (left, right) ->
      fprintf pp_format "%a < %a" pp_expession left pp_expession right
  | More (left, right) ->
      fprintf pp_format "%a > %a" pp_expession left pp_expession right
  | LessOrEqual (left, right) ->
      fprintf pp_format "%a <= %a" pp_expession left pp_expession right
  | MoreOrEqual (left, right) ->
      fprintf pp_format "%a >= %a" pp_expession left pp_expession right
  | Equal (left, right) ->
      fprintf pp_format "%a == %a" pp_expession left pp_expession right
  | NotEqual (left, right) ->
      fprintf pp_format "%a != %a" pp_expession left pp_expession right
  | PostInc operand -> fprintf pp_format "%a++" pp_expession operand
  | PrefInc operand -> fprintf pp_format "++%a" pp_expession operand
  | PostDec operand -> fprintf pp_format "%a--" pp_expession operand
  | PrefDec operand -> fprintf pp_format "--%a" pp_expession operand
  | Null -> fprintf pp_format "null"
  | This -> fprintf pp_format "this"
  | Base -> fprintf pp_format "base"
  | Value value -> fprintf pp_format "%a" pp_value value
  | Identifier id -> fprintf pp_format "%s" id
  | AccessByPoint (left, right) ->
      if was_creation left then
        fprintf pp_format "%a.%a" pp_expession_parens left pp_expession right
      else fprintf pp_format "%a.%a" pp_expession left pp_expession right
  | ArrayAccess (array, index) ->
      fprintf pp_format "%a[%a]" pp_expession array pp_expession index
  | ArrayCreationWithSize (array_type, size) ->
      fprintf pp_format "new %a[%a]" pp_type array_type pp_expession size
  | ArrayCreationWithElements (array_type, expression_list) ->
      fprintf pp_format "new %a[] { %a }" pp_type array_type pp_expession_list
        expression_list
  | ClassCreation (name, expression_list) ->
      fprintf pp_format "new %a(%a)" pp_name name pp_expession_list
        expression_list
  | CallMethod (method_expression, expression_list) ->
      fprintf pp_format "%a(%a)" pp_expession method_expression
        pp_expession_list expression_list
  | Assign (left, right) ->
      fprintf pp_format "%a = %a" pp_expession left pp_expession right

and pp_expession_parens pp_format = fprintf pp_format "(%a)" pp_expession

and pp_expession_list pp_format =
  pp_print_list ~pp_sep:pp_sep_comma pp_expession pp_format

let pp_pairs_decl_list pp_format =
  let pp_pair pp_format = function
    | Name name, None -> fprintf pp_format "%s" name
    | Name name, Some expression ->
        fprintf pp_format "%s = %a" name pp_expession expression in
  pp_print_list ~pp_sep:pp_sep_comma pp_pair pp_format

let rec pp_statement pp_format = function
  | Expression expression -> fprintf pp_format "%a;" pp_expession expression
  | If (condition, then_statement, else_statement_opt) -> (
      pp_sep_endline pp_format () ;
      match else_statement_opt with
      | Some else_statetment -> (
        match then_statement with
        | StatementBlock _ ->
            fprintf pp_format "if (%a) %a@;<0 0>else %a" pp_expession condition
              pp_statement then_statement pp_statement else_statetment
        | _ ->
            fprintf pp_format "if (%a) %a else %a" pp_expession condition
              pp_statement then_statement pp_statement else_statetment )
      | None ->
          fprintf pp_format "if (%a) %a" pp_expession condition pp_statement
            then_statement )
  | While (condition, body) ->
      pp_sep_endline pp_format () ;
      fprintf pp_format "while (%a) %a" pp_expession condition pp_statement body
  | For (decl_statement_opt, condition_opt, after_expression_list, body) ->
      pp_sep_endline pp_format () ;
      ( match decl_statement_opt with
      | Some decl_statement ->
          fprintf pp_format "for (%a" pp_statement decl_statement
      | None -> fprintf pp_format "for ( ;" ) ;
      ( match condition_opt with
      | Some condition -> fprintf pp_format " %a;" pp_expession condition
      | None -> fprintf pp_format " ;" ) ;
      fprintf pp_format " %a) " pp_expession_list after_expression_list ;
      fprintf pp_format " %a" pp_statement body
  | Break -> fprintf pp_format "break;"
  | Continue -> fprintf pp_format "continue;"
  | Return expresion_opt -> (
    match expresion_opt with
    | Some expr -> fprintf pp_format "return %a;" pp_expession expr
    | None -> fprintf pp_format "return;" )
  | VariableDecl (modifier_o, var_type, pairs_list) ->
      ( match modifier_o with
      | Some modifier -> fprintf pp_format "%a " pp_modifier modifier
      | None -> () ) ;
      fprintf pp_format "%a " pp_type var_type ;
      fprintf pp_format "%a;" pp_pairs_decl_list pairs_list
  | StatementBlock statement_list ->
      fprintf pp_format "@;<0 0>@[<v 2>{@;<0 0>%a@;<0 -2>}@]" pp_statement_list
        statement_list

and pp_statement_list pp_format =
  pp_print_list ~pp_sep:pp_sep_endline pp_statement pp_format

let pp_type_name_pairs_list pp_format =
  let pp_type_name_pair pp_format = function
    | typeq, name -> fprintf pp_format "%a %a" pp_type typeq pp_name name in
  pp_print_list ~pp_sep:pp_sep_comma pp_type_name_pair pp_format

let pp_class_element pp_format = function
  | modifier_list, Method (method_type, method_name, arguments, body_o) -> (
    match body_o with
    | Some body ->
        fprintf pp_format "%a" pp_modifier_list modifier_list ;
        pp_sep_space pp_format () ;
        fprintf pp_format "%a %a(%a) %a" pp_type method_type pp_name method_name
          pp_type_name_pairs_list arguments pp_statement body
    | None ->
        fprintf pp_format "%a" pp_modifier_list modifier_list ;
        pp_sep_space pp_format () ;
        fprintf pp_format "%a %a(%a);" pp_type method_type pp_name method_name
          pp_type_name_pairs_list arguments )
  | modifier_list, Constructor (name, arguments, call_constructor, body) -> (
    match call_constructor with
    | Some call_cnstr ->
        fprintf pp_format "%a %a(%a) : %a %a" pp_modifier_list modifier_list
          pp_name name pp_type_name_pairs_list arguments pp_expession call_cnstr
          pp_statement body
    | None ->
        fprintf pp_format "%a %a(%a) %a" pp_modifier_list modifier_list pp_name
          name pp_type_name_pairs_list arguments pp_statement body )
  | modifier_list, Field (field_type, field_expressions) ->
      fprintf pp_format "%a %a %a;" pp_modifier_list modifier_list pp_type
        field_type pp_pairs_decl_list field_expressions

let pp_class_elements pp_format =
  pp_print_list ~pp_sep:pp_sep_two_endline pp_class_element pp_format

let pp_class_decl pp_format = function
  | Class (modifier_list, name, parent_name_opt, elements) ->
      fprintf pp_format "%a" pp_modifier_list modifier_list ;
      ( match modifier_list with
      | [] -> fprintf pp_format ""
      | _ -> fprintf pp_format " " ) ;
      fprintf pp_format "class %a" pp_name name ;
      ( match parent_name_opt with
      | Some parent_name -> fprintf pp_format " : %a" pp_name parent_name
      | None -> () ) ;
      fprintf pp_format "@;<0 0>@[<v 2>{@;<0 0>%a@;<0 -2>}@]" pp_class_elements
        elements

let pp_class_decl_list pp_format =
  let pp_cdl ppf = pp_print_list ~pp_sep:pp_sep_two_endline pp_class_decl ppf in
  fprintf pp_format "@[<v 0>%a@]@;<0 0>@;<0 0>" pp_cdl

let pretty_print = pp_class_decl_list std_formatter
