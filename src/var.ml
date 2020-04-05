type t = Var of int

let of_int i = Var i

let to_int (Var i) = i

let to_string (Var i) = "#" ^ string_of_int i
