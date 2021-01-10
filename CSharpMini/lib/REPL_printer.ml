open Hashtbl_impr
open Interpreter.Interpretation (Interpreter.Result)

let get_buffer = Buffer.create 1024

let show_table ht pp_key pp_value =
  match Hashtbl.length ht with
  | 0 -> "[[]]"
  | _ ->
      let buffer = Buffer.create 8192 in
      Printf.bprintf buffer "[[" ;
      Hashtbl.iter
        (fun key value ->
          Printf.bprintf buffer "%a -> %a\n" pp_key key pp_value value)
        ht ;
      Printf.bprintf buffer "]]" ;
      Buffer.contents buffer

let show_value ht show_value =
  let buffer = Buffer.create 8192 in
  Hashtbl.iter
    (fun _ value ->
      Buffer.add_string buffer (show_value value) ;
      Buffer.add_string buffer "\n")
    ht ;
  Buffer.contents buffer

let pp_key buffer key = Buffer.add_string buffer key
let pp_value buffer value = Buffer.add_string buffer (show_variable value)
let pp_table ht pp_key pp_value = pp pp_key pp_value Format.std_formatter ht
