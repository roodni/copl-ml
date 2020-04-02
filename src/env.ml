open Var
open Value

type env = (var * value) list

let rec env_to_string env =
  let assign_to_string (var, value) =
    Printf.sprintf "%s = %s" (var_to_string var) (value_to_string value)
  in
  match env with
  | [] -> ""
  | [ assign ] -> assign_to_string assign
  | assign :: env' ->
      Printf.sprintf "%s, %s" (env_to_string env') (assign_to_string assign)
