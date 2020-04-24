open Printf

type t = Bool | Int | Fun of t * t | List of t | Var of Tvar.t

let rec to_string = function
  | Bool -> "bool"
  | Int -> "int"
  | Fun (a, b) -> sprintf "(%s -> %s)" (to_string a) (to_string b)
  | List t -> sprintf "%s list" (to_string t)
  | Var t -> Tvar.to_string t

let compare a b = compare a b
