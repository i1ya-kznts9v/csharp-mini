open Lambda_lib.Lambda

type 'a status = Done of 'a | WIP of 'a

let fin x = Done x
let wip x = WIP x

let nor_small_step_strat =
  let rec helper = function
    | (Var _ as l) | (Abs (_, _) as l) -> fin l
    | App (f, arg) -> (
      match helper f with
      | WIP f2 -> fin (app f2 arg)
      | Done (Abs (x, e)) -> wip (subst x ~by:arg e)
      | Done f2 -> fin (App (f2, arg)) ) in
  let rec loop t =
    match helper t with
    | Done x -> x
    | WIP x ->
        Format.printf " -- %a\n%!" pp_lam x;
        loop x in
  let on_app _ f arg = loop (app f arg) in
  let on_abs _ f x = loop (abs f x) in
  let on_var _ x = loop (var x) in
  {on_var; on_abs; on_app}

let ( <| ) = ( @@ )

let test_fac =
  let zero = abs "g" @@ abs "y" @@ Var "y" in
  let one = abs "f" @@ abs "x" @@ app f (Var "x") in
  let two = abs "f" @@ abs "x" @@ app f (app f x) in
  let three = abs "f" @@ abs "x" @@ app f (app f (app f x)) in
  let plus =
    abs "m" @@ abs "n" @@ abs "f" @@ abs "x" @@ app (app m f) (app (app n f) x)
  in
  let mul = abs "x" @@ abs "y" @@ abs "z" @@ app x (app y z) in
  let true_ = abs "x" @@ abs "y" @@ Var "x" in
  let false_ = abs "x" @@ abs "y" @@ Var "y" in
  let isZero = abs "n" @@ app (app n (abs "x" false_)) true_ in
  (* if-then-else for lazy strategy *)
  let ite cond th el = app (app (app isZero cond) th) el in
  let pred =
    let xxx = abs "g" @@ abs "h" @@ app h (app g f) in
    abs "n" @@ abs "f" @@ abs "x"
    @@ app (app (app n xxx) (abs "u" x)) (abs "u" (Var "u")) in
  let fact =
    abs "self" @@ abs "N"
    @@ ite (Var "N") one
         (app (app mul (app (var "self") (app pred (var "N")))) (var "N")) in
  let ygrek =
    let hack = abs "x" (app f (app x x)) in
    abs "f" (app hack hack) in
  (* 5! = 120 *)
  let () =
    test nor_strat @@ app (app ygrek fact) (app (app plus two) three)
    |> fun lam -> Format.printf "%a\n%!" pp_lam lam in
  ()
