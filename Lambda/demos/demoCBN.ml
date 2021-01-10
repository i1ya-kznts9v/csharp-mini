open Lambda_lib.Lambda

(* TODO: not really implemented *)
let cbn_small_step_strat =
  let open Result in
  let rec helper = function
    | Var _ as l -> Result.Ok l
    | Abs (_, _) as l -> Result.Ok l
    | App (f, arg) -> (
      match helper f with
      | Error f2 -> Ok (App (f2, arg))
      | Ok (Abs (x, e)) -> Error (subst x ~by:arg e)
      | Ok f2 -> Ok (App (f2, arg)) ) in
  let rec main t =
    match helper t with
    | Ok x -> x
    | Error x ->
        Format.printf " -- %a\n%!" pp_lam x;
        main x in
  let on_app _st f arg = main (App (f, arg)) in
  let on_abs _st f x = main (Abs (f, x)) in
  let on_var _st x = main (Var x) in
  {on_var; on_abs; on_app}
