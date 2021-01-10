open Csharpmini_lib.Ast
open Csharpmini_lib.Parser
open Csharpmini_lib.REPL_printer
open Csharpmini_lib.REPL_stdlib
open Csharpmini_lib.Interpreter
open Csharpmini_lib.Interpreter.ClassLoader (Csharpmini_lib.Interpreter.Result)

open Csharpmini_lib.Interpreter.Interpretation
       (Csharpmini_lib.Interpreter.Result)

exception Incorrect_input of string
exception Incorrect_state of string
exception Evaluation_error of string

let check_end_input str =
  let reg_expr = Str.regexp {|.*#[ ]*$|} in
  Str.string_match reg_expr str 0

let check_on_symbol str =
  match String.rindex_opt str '#' with
  | Some position -> Str.string_before str position
  | None ->
      raise (Incorrect_input "The input line must contain the '#' character!")

let rec repl_eval buffer class_table input_ctx k_table =
  print_string "> " ;
  let read_str = read_line () in
  match check_end_input read_str with
  | false ->
      Buffer.add_string buffer read_str ;
      repl_eval buffer class_table input_ctx k_table
  | true -> (
      Buffer.add_string buffer (check_on_symbol read_str) ;
      let input_processing str =
        match apply Class.method_decl str with
        | Some (Method (method_type, Name method_name, args, body)) -> (
            let new_key = make_method_key method_name args in
            if body = None then (
              Buffer.clear buffer ;
              raise (Incorrect_input "Repl method cannot be abstract") )
            else
              match Hashtbl.find_opt class_table "Repl" with
              | None ->
                  Buffer.clear buffer ;
                  raise (Incorrect_state "Repl class not found")
              | Some repl_class ->
                  let create_new_method =
                    { method_type
                    ; is_abstract= false
                    ; is_virtual= false
                    ; is_override= false
                    ; arguments= args
                    ; key= new_key
                    ; body
                    ; is_overriden= false } in
                  if not (Hashtbl.mem repl_class.methods_table new_key) then (
                    Hashtbl.add k_table new_key str ;
                    Hashtbl.add repl_class.methods_table new_key
                      create_new_method )
                  else (
                    Hashtbl.replace k_table new_key str ;
                    Hashtbl.replace repl_class.methods_table new_key
                      create_new_method ) ;
                  Hashtbl.replace class_table "Repl"
                    {repl_class with methods_table= repl_class.methods_table} ;
                  prerr_endline "New method has been added" ;
                  Buffer.clear buffer ;
                  repl_eval buffer class_table input_ctx k_table )
        | Some _ ->
            Buffer.clear buffer ;
            raise (Incorrect_state "Not method declaration error")
        | None -> (
          match apply Statement.statement str with
          | Some stat -> (
            match eval_stmt stat input_ctx class_table with
            | Error m -> Buffer.clear buffer ; raise (Evaluation_error m)
            | Ok eval_stat_ctx ->
                print_endline "Statement interpreted" ;
                Buffer.clear buffer ;
                repl_eval buffer class_table eval_stat_ctx k_table )
          | None -> (
            match apply Expression.expression str with
            | Some parse_expr -> (
              match eval_expr parse_expr input_ctx class_table with
              | Error m -> Buffer.clear buffer ; raise (Evaluation_error m)
              | Ok eval_ctx ->
                  print_endline
                    ("Result: " ^ show_values eval_ctx.last_expr_result) ;
                  Buffer.clear buffer ;
                  repl_eval buffer class_table eval_ctx k_table )
            | None ->
                Buffer.clear buffer ;
                raise (Incorrect_input "Incorrect input") ) ) in
      match check_on_symbol read_str with
      | "show_stdlib" ->
          print_endline "Stdlib classes:" ;
          prerr_endline stdlib_storage ;
          Buffer.clear buffer ;
          repl_eval buffer class_table input_ctx k_table
      | "exit" -> ()
      | "show_variables" ->
          print_endline "Table of variables:" ;
          print_endline (show_table input_ctx.var_table pp_key pp_value) ;
          Buffer.clear buffer ;
          repl_eval buffer class_table input_ctx k_table
      | "show_methods" -> (
        match Hashtbl.find_opt class_table "Repl" with
        | None -> print_endline "Repl class not found"
        | Some _ ->
            print_endline "Available methods:" ;
            print_endline (show_value k_table show_key_t) ;
            Buffer.clear buffer ;
            repl_eval buffer class_table input_ctx k_table )
      | "show_context" ->
          print_endline "Current context:" ;
          print_endline (show_context input_ctx ^ "\n") ;
          Buffer.clear buffer ;
          repl_eval buffer class_table input_ctx k_table
      | _ -> (
        try input_processing (Buffer.contents buffer)
        with Incorrect_state m | Incorrect_input m | Evaluation_error m ->
          print_endline m ;
          Buffer.clear buffer ;
          repl_eval buffer class_table input_ctx k_table ) )

let () =
  print_endline
    "-_-_-_-_-_-_-_-_-_-_- Welcome to CSharpMini REPL -_-_-_-_-_-_-_-_-_-_-"

let () = print_endline "You can write expression, statement, or method"
let () = print_endline "At the end of your input you need to type '#' character"

let () =
  print_endline
    "Manual:\n\
    \ --- show_stdlib# for get available classes in standart library\n\
    \ --- show_variables# to get all information about local variables\n\
    \ --- show_methods# for get available methods\n\
    \ --- show_context# to get context\n\
    \ --- exit# to leave REPL"

let repl_start =
  match stdlib_classes with
  | None -> print_endline "Syntax Error! \n"
  | Some cl_list -> (
    match load cl_list (Hashtbl.create 128) with
    | Error m -> print_endline ("Error in loading standart library: " ^ m)
    | Ok new_table ->
        let repl_ctx =
          { cur_object=
              ObjectReference
                { class_key= "Repl"
                ; field_references_table= Hashtbl.create 128
                ; number= 0 }
          ; var_table= Hashtbl.create 128
          ; last_expr_result= VVoid
          ; runtime_signal= NoSignal
          ; curr_method_type= TVoid
          ; is_main_scope= false
          ; nested_loops_cnt= 0
          ; scope_level= 0
          ; cur_constr_key= None
          ; prev_context= None
          ; obj_created_cnt= 0
          ; is_creation= false
          ; constr_affilation= None } in
        repl_eval (Buffer.create 1024) new_table repl_ctx (Hashtbl.create 1024)
    )
