type t = Var of string

let of_string s = Var s

let to_string v =
  let (Var s) = v in
  s
