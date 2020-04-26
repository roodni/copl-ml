open Printf

type t =
  | Int of int
  | Bool of bool
  | Fun of env * Var.t * Expr.t
  | RecFun of env * Var.t * Var.t * Expr.t
  | Loc of Loc.t
  | Nil
  | Cons of t * t

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
  | Nil -> "[]"
  | Cons (l, r) ->
      let ls = to_string l in
      sprintf "%s :: %s"
        (match l with Cons _ -> "(" ^ ls ^ ")" | _ -> ls)
        (to_string r)

and env_to_string env =
  let bind_to_string (var, value) =
    Printf.sprintf "%s = %s" (Var.to_string var) (to_string value)
  in
  env |> List.rev_map bind_to_string |> String.concat ", "
