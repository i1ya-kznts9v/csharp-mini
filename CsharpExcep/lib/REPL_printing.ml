open Hashtbl_der
open Interpreter.Interpreter (Interpreter.Result)

let create_buffer = Buffer.create 1000
let ppb_table_key buf k = Buffer.add_string buf k
let ppb_variable buf v = Buffer.add_string buf (show_variable v)

let show_table hashtable pp_key pp_val =
  match Hashtbl.length hashtable with
  | 0 -> "[[]]"
  | _ ->
      let buf = Buffer.create 10000 in
      Printf.bprintf buf "[[" ;
      Hashtbl.iter
        (fun key value ->
          Printf.bprintf buf "%a -> %a\n" pp_key key pp_val value)
        hashtable ;
      Printf.bprintf buf "]]" ;
      Buffer.contents buf

let show_values hashtable show_value =
  let buf = Buffer.create 10000 in
  Hashtbl.iter
    (fun _ value ->
      Buffer.add_string buf (show_value value) ;
      Buffer.add_string buf "\n")
    hashtable ;
  Buffer.contents buf

let print_table hashtable pp_s pp_elem =
  pp pp_s pp_elem Format.std_formatter hashtable
