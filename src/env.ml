open Value

type t = (Var.t * value) list

let rec to_string env =
  let assign_to_string (var, value) =
    Printf.sprintf "%s = %s" (Var.to_string var) (value_to_string value)
  in
  match env with
  | [] -> ""
  | [ assign ] -> assign_to_string assign
  | assign :: env' ->
      Printf.sprintf "%s, %s" (to_string env') (assign_to_string assign)
