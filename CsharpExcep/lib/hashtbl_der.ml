(* module type Hashtbl_der = sig
  type ('a, 'b) t

  val create : ?random:bool -> int -> ('a, 'b) t

  val pp :
       (Format.formatter -> 'a -> unit)
    -> (Format.formatter -> 'b -> unit)
    -> Format.formatter
    -> ('a, 'b) t
    -> unit

  val get_value_option : ('a, 'b) t -> 'a -> 'b option
  val hash_filter : ('a, 'b) t -> ('a -> 'b -> bool) -> ('a, 'b) t
end *)

type ('a, 'b) t = ('a, 'b) Hashtbl.t

let create = Hashtbl.create

let pp pp_key pp_value ppf hashtable =
  Format.fprintf ppf "[["
  |> fun () ->
  Hashtbl.iter
    (fun key data ->
      Format.fprintf ppf "@[<1>%a@ ->@ %a@]@\n@." pp_key key pp_value data)
    hashtable
  |> fun () -> Format.fprintf ppf "]]@\n"

let filter : ('a, 'b) t -> ('a -> 'b -> bool) -> ('a, 'b) t =
 fun ht f ->
  let new_table = Hashtbl.create 100 in
  Hashtbl.iter (fun k v -> if f k v then Hashtbl.add new_table k v) ht ;
  new_table

let get_value_option hashtable key = Hashtbl.find_opt hashtable key

(*удалить!*)
