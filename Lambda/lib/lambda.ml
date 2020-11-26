type name = string
type t = Var of name | Abs of name * t | App of t * t

(* smart constructors *)
let var x = Var x
let abs x y = Abs (x, y)
let app x y = App (x, y)

(* TODO: use a set instead of list *)
let list_remove x = List.filter (fun a -> a <> x)

let free_vars =
  let rec helper acc = function
    | Var s -> s :: acc
    | Abs (s, l) -> acc @ list_remove s (helper [] l)
    | App (l, r) -> helper (helper acc r) l in
  helper []

let is_free_in x term = List.mem x (free_vars term)

let pp_lam =
  let mangle t fmt x =
    if is_free_in x t then Format.fprintf fmt "%s" x else Format.fprintf fmt "_"
  in
  let rec pp fmt = function
    | Var s -> Format.fprintf fmt "%s" s
    | App (l, r) -> Format.fprintf fmt "(%a %a)" pp l pp r
    | Abs (x, Abs (y, Var z)) when x = z && y <> z -> Format.fprintf fmt "⊤"
    | Abs (x, Abs (y, Var z)) when y = z && x <> z -> Format.fprintf fmt "⊥"
    | Abs (f, Abs (x, Var z)) when x = z && x <> f -> Format.fprintf fmt "0"
    | Abs (f, Abs (x, App (Var g, Var z))) when x = z && x <> f && g = f ->
        Format.fprintf fmt "1"
    | Abs (f, Abs (x, App (Var g, App (Var h, Var z))))
      when x = z && x <> f && g = f && h = g ->
        Format.fprintf fmt "2"
    | Abs (v1, Abs (v2, Abs (v3, Abs (v4, t)))) ->
        Format.fprintf fmt "(λ %a %a %a %a -> %a)" (mangle t) v1 (mangle t) v2
          (mangle t) v3 (mangle t) v4 pp t
    | Abs (v1, Abs (v2, Abs (v3, t))) ->
        Format.fprintf fmt "(λ %a %a %a -> %a)" (mangle t) v1 (mangle t) v2
          (mangle t) v3 pp t
    | Abs (v1, Abs (v2, t)) ->
        Format.fprintf fmt "(λ %a %a -> %a)" (mangle t) v1 (mangle t) v2 pp t
    | Abs (x, t) -> Format.fprintf fmt "(λ %a -> %a)" (mangle t) x pp t in
  pp

let replace_name x ~by =
  let rec helper = function
    | Var y when x = y -> Var by
    | Var t -> Var t
    | App (l, r) -> App (helper l, helper r)
    | Abs (y, t) when x = y -> Abs (by, helper t)
    | Abs (z, t) -> Abs (z, helper t) in
  helper

let rec next_name s old = if List.mem s old then next_name ("_" ^ s) old else s

(*  The call [subst x ~by:v e] means `[x/v]e` or `e[v -> x]` *)
let subst x ~by:v =
  let rec helper e =
    match e with
    | Var y when y = x -> v
    | Var y -> Var y
    | App (l, r) -> app (helper l) (helper r)
    | Abs (y, b) when y = x -> abs y b
    | Abs (y, t) when is_free_in y v ->
        let frees = free_vars v @ free_vars t in
        let w = next_name y frees in
        helper (abs w (replace_name y ~by:w t))
    | Abs (y, b) -> abs y (helper b) in
  helper

type strat =
  { on_var: strat -> name -> t
  ; on_abs: strat -> name -> t -> t
  ; on_app: strat -> t -> t -> t }

let apply_strat st = function
  | Var name -> st.on_var st name
  | Abs (x, b) -> st.on_abs st x b
  | App (l, r) -> st.on_app st l r

let without_strat =
  let on_var _ = var in
  let on_abs _ = abs in
  let on_app _ = app in
  {on_var; on_abs; on_app}

let cbn_strat =
  let on_app st f arg =
    match apply_strat st f with
    | Abs (x, e) -> apply_strat st (subst x ~by:arg e)
    | f2 -> App (f2, arg) in
  {without_strat with on_app}

let under_abstraction st x b = abs x (apply_strat st b)

(* Normal Order Reduction to Normal Form
   Application function reduced as CBN first
   + Reduce under abstractions *)
let nor_strat =
  let on_app st f arg =
    match apply_strat cbn_strat f with
    | Abs (x, e) -> apply_strat st @@ subst x ~by:arg e
    | f1 ->
        let f2 = apply_strat st f1 in
        let arg2 = apply_strat st arg in
        App (f2, arg2) in
  {without_strat with on_app; on_abs= under_abstraction}

(* Call-by-Value Reduction to Weak Normal Form *)
let cbv_strat =
  let on_app st f arg =
    match apply_strat st f with
    | Abs (x, e) ->
        let arg2 = apply_strat st arg in
        apply_strat st @@ subst x ~by:arg2 e
    | f2 -> App (f2, apply_strat st arg) in
  {without_strat with on_app}

(* Applicative Order Reduction to Normal Form
   As CBV but reduce under abstractions *)
let ao_strat = {cbv_strat with on_abs= under_abstraction}
let a = var "a"
let x = var "x"
let y = var "y"
let z = var "z"
let f = var "f"
let g = var "g"
let h = var "h"
let m = var "m"
let n = var "n"
let p = var "p"
let zero = abs "f" @@ abs "x" x
let one = abs "f" @@ abs "x" @@ app f x
let two = abs "f" @@ abs "x" @@ app f (app f x)
let three = abs "f" @@ abs "x" @@ app f (app f (app f x))

let test strat term =
  Format.printf "Evaluating: %a\n%!" pp_lam term ;
  let rez = apply_strat strat term in
  Format.printf "Result:     %a\n%!" pp_lam rez ;
  rez
