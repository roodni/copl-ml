type var = Var of string

let var_to_string v =
  let (Var s) = v in
  s
