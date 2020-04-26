open Base

type t =
  | Int of int
  | Bool of bool
  | Fun of env * Var.t * Expr.t
  | RecFun of env * Var.t * Var.t * Expr.t
  | Loc of Loc.t
  | Nil
  | Cons of t * t

and env = (Var.t * t) list

val to_string : t -> string

val env_to_string : env -> string
