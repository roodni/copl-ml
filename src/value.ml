open Expr
open Printf

type value =
  | IntVal of int
  | BoolVal of bool
  | FunVal of env * expr
  | RecFunVal of env * expr

and env = value list

let rec value_to_string = function
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | FunVal (env, expr) ->
      sprintf "(%s)[fun . -> %s]" (env_to_string env) (expr_to_string expr)
  | RecFunVal (env, expr) ->
      sprintf "(%s)[rec . = fun . -> %s]" (env_to_string env)
        (expr_to_string expr)

and env_to_string env =
  match env with
  | [] -> ""
  | [ v ] -> value_to_string v
  | v :: env' ->
      Printf.sprintf "%s, %s" (env_to_string env') (value_to_string v)
