open Printf

type t =
  | Int of int
  | Bool of bool
  | Fun of env * Var.t * Expr.t
  | RecFun of env * Var.t * Var.t * Expr.t
  | Loc of Loc.t

and env = (Var.t * t) list

let rec to_string = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Fun (env, v, expr) ->
      sprintf "(%s)[fun %s -> %s]" (env_to_string env) (Var.to_string v)
        (Expr.to_string expr)
  | RecFun (env, f, a, expr) ->
      sprintf "(%s)[rec %s = fun %s -> %s]" (env_to_string env)
        (Var.to_string f) (Var.to_string a) (Expr.to_string expr)
  | Loc l -> Loc.to_string l

and env_to_string env =
  let bind_to_string (var, value) =
    Printf.sprintf "%s = %s" (Var.to_string var) (to_string value)
  in
  match env with
  | [] -> ""
  | [ bind ] -> bind_to_string bind
  | bind :: env' ->
      Printf.sprintf "%s, %s" (env_to_string env') (bind_to_string bind)
