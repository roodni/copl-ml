open Expr
open Printf

type value =
  | IntVal of int
  | BoolVal of bool
  | FunVal of env * Var.t * expr
  | RecFunVal of env * Var.t * Var.t * expr
  | LocVal of Loc.t

and env = (Var.t * value) list

let rec value_to_string = function
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | FunVal (env, v, expr) ->
      sprintf "(%s)[fun %s -> %s]" (env_to_string env) (Var.to_string v)
        (expr_to_string expr)
  | RecFunVal (env, f, a, expr) ->
      sprintf "(%s)[rec %s = fun %s -> %s]" (env_to_string env)
        (Var.to_string f) (Var.to_string a) (expr_to_string expr)
  | LocVal l -> Loc.to_string l

and env_to_string env =
  let bind_to_string (var, value) =
    Printf.sprintf "%s = %s" (Var.to_string var) (value_to_string value)
  in
  match env with
  | [] -> ""
  | [ bind ] -> bind_to_string bind
  | bind :: env' ->
      Printf.sprintf "%s, %s" (env_to_string env') (bind_to_string bind)
