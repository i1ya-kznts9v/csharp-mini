open Csharpmini_lib.Ast
open Csharpmini_lib.Parser
open Opal

let list_no_option list = match list with Some x -> x | None -> []

(* let list_with_option list = match list with [] -> None | x -> Some x *)

let rec print_list = function
  | [] -> print_string ""
  | hd :: tl ->
      print_string (show_cs_class hd) ;
      print_endline "" ;
      print_list tl

let parse_result = list_no_option (parse parser (LazyStream.of_string {|
|}))
