open Opal
open AST

let not_space c = match c with
  | ' ' -> true
  | _ -> false

let digit_c =
  let is_digit ch =
    let c = Char.code ch in           
    Char.code '0' <= c && c <= Char.code '9'
  in
  satisfy is_digit
  
module Expr = struct
open AST
  let null = token "null" >> return (Null) 
  let super = token "base" >> return (Base)
  let this = token "this" >> return (This)
end