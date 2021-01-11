open Csharp_lib.REPL_printing
open Csharp_lib.Standlib
open Csharp_lib.Parser
open Csharp_lib.Ast
open Csharp_lib.Interpreter
open Csharp_lib.Interpreter.ClassLoader (Csharp_lib.Interpreter.Result)
open Csharp_lib.Interpreter.Interpreter (Csharp_lib.Interpreter.Result)

exception Incorrect_input of string
exception Incorrect_state of string
exception Evaluation_error of string

let check_end_input str =
  let r = Str.regexp {|.*@[ ]*$|} in
  Str.string_match r str 0

let check_on_symbol str =
  match String.rindex_opt str '@' with
  | Some position -> Str.string_before str position
  | None ->
      raise (Invalid_argument "The input line must contain the '@' character! ")

let rec repl_eval buffer class_table input_ctx k_table =
  print_string "REPL # " ;
  let read_str = read_line () in
  match check_end_input read_str with
  (*false - ввод пользователя не закончен, добавляем в буффер*)
  | false ->
      Buffer.add_string buffer read_str ;
      repl_eval buffer class_table input_ctx k_table
  | true -> (
      Buffer.add_string buffer (check_on_symbol read_str) ;
      let input_processing str =
        match apply_parser class_method str with
        | Some (Method (method_type, method_name, args, body)) -> (
            let new_key = method_make_key method_name args in
            match Hashtbl.find_opt class_table "Repl" with
            | None ->
                Buffer.clear buffer ;
                raise (Incorrect_state "Repl class not found")
            | Some repl_class ->
                let create_new_method =
                  { method_type
                  ; has_override= false
                  ; has_static_mod= false
                  ; args
                  ; method_key= new_key
                  ; body } in
                if not (Hashtbl.mem repl_class.method_table new_key) then (
                  Hashtbl.add k_table new_key str ;
                  Hashtbl.add repl_class.method_table new_key create_new_method
                  )
                else (
                  Hashtbl.replace k_table new_key str ;
                  Hashtbl.replace repl_class.method_table new_key
                    create_new_method ) ;
                Hashtbl.replace class_table "Repl"
                  {repl_class with method_table= repl_class.method_table} ;
                prerr_endline "New method has been added" ;
                Buffer.clear buffer ;
                repl_eval buffer class_table input_ctx k_table )
        | Some _ ->
            Buffer.clear buffer ;
            raise (Incorrect_state "Not method declaration error")
        | None -> (
          match apply_parser Stat.parse_statement str with
          | Some stat -> (
            match interprete_stat stat input_ctx class_table with
            | Error m -> Buffer.clear buffer ; raise (Evaluation_error m)
            | Ok eval_stat_ctx when eval_stat_ctx.runtime_flag = NoFlag ->
                print_endline "Statement interpreted" ;
                Buffer.clear buffer ;
                repl_eval buffer class_table eval_stat_ctx k_table
            | Ok _ ->
                Buffer.clear buffer ;
                raise (Evaluation_error "Unhandled exception") )
          | None -> (
            match apply_parser Expr.expr str with
            | Some parse_expr -> (
              match interprete_expr parse_expr input_ctx class_table with
              | Error m -> Buffer.clear buffer ; raise (Evaluation_error m)
              | Ok eval_ctx ->
                  print_endline
                    ("Result: " ^ show_value eval_ctx.last_expr_result) ;
                  Buffer.clear buffer ;
                  repl_eval buffer class_table eval_ctx k_table )
            | None ->
                Buffer.clear buffer ;
                raise (Incorrect_input "Incorrect input") ) ) in
      match check_on_symbol read_str with
      | "show_stdlib" ->
          print_endline "Stdlib classes:" ;
          prerr_endline standlib_classes ;
          Buffer.clear buffer ;
          repl_eval buffer class_table input_ctx k_table
      | "exit" -> ()
      | "show_variable_table" ->
          print_endline "Table of variables:" ;
          print_endline
            (show_table input_ctx.variable_table ppb_table_key ppb_variable) ;
          Buffer.clear buffer ;
          repl_eval buffer class_table input_ctx k_table
      | "show_methods" -> (
        match Hashtbl.find_opt class_table "Repl" with
        | None -> print_endline "Repl class not found"
        | Some _ ->
            print_endline "Available methods:" ;
            print_endline (show_values k_table show_table_key) ;
            Buffer.clear buffer ;
            repl_eval buffer class_table input_ctx k_table )
      | "show_context" ->
          print_endline "Current context:" ;
          Buffer.clear buffer ;
          print_endline (show_context input_ctx ^ "\n")
      | _ -> (
        try input_processing (Buffer.contents buffer) with
        | Incorrect_state m -> print_endline m
        | Incorrect_input m | Evaluation_error m ->
            print_endline m ;
            Buffer.clear buffer ;
            repl_eval buffer class_table input_ctx k_table ) )

let () =
  print_endline
    "-_-_-_-_-_-_-_-_-_-_- Welcome to Csharp REPL -_-_-_-_-_-_-_-_-_-_-\n\n"

let () = print_endline "You can write statement, expression, or new method."

let () =
  print_endline "At the end of your input you need to type '@' character."

let () =
  prerr_endline
    "If you want to submit several expressions at once, then you need to wrap \
     them in {}.\n\
     For example: {Console.WriteLine (1); Console.WriteLine (1);} @"

let () =
  print_endline
    "Special commands:\n\
    \ --- show_variable_table@ for showing all information about local variables\n\
    \ --- show_stdlib@ for showing current available classes in standart library\n\
    \ --- show_methods@ for showing current available methods\n\
    \ --- show_context@ for showing current context\n\
    \ --- exit@ for exiting REPL"

let repl_start =
  match parse_std_cl with
  | None -> print_endline "Syntax Error! \n"
  | Some cl_list -> (
    match load_classes cl_list (Hashtbl.create 128) with
    | Error m -> print_endline ("Error in loading standart library: " ^ m)
    | Ok new_table ->
        let repl_ctx =
          { current_o=
              ObjRef
                { class_key= "Repl"
                ; parent_key= None
                ; class_table= Hashtbl.create 128
                ; number= 0 }
          ; variable_table= Hashtbl.create 128
          ; last_expr_result= VVoid
          ; runtime_flag= NoFlag
          ; current_meth_type= Void
          ; is_main= false
          ; curr_constructor= None
          ; prev_ctx= None
          ; visibility_level= 0
          ; count_of_nested_cycle= 0
          ; count_of_obj= 0
          ; is_creation= false } in
        repl_eval (Buffer.create 1000) new_table repl_ctx (Hashtbl.create 1000)
    )
