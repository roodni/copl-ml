open Expr
open Printf

type value = IntVal of int | BoolVal of bool | FunVal of env * Var.t * expr

and env = (Var.t * value) list

let rec value_to_string = function
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | FunVal (env, v, expr) ->
      sprintf "(%s)[fun %s -> %s]" (env_to_string env) (Var.to_string v)
        (expr_to_string expr)

and env_to_string env =
  let assign_to_string (var, value) =
    Printf.sprintf "%s = %s" (Var.to_string var) (value_to_string value)
  in
  match env with
  | [] -> ""
  | [ assign ] -> assign_to_string assign
  | assign :: env' ->
      Printf.sprintf "%s, %s" (env_to_string env') (assign_to_string assign)
