type t =
  | Int of int
  | Bool of bool
  | Fun of env * Var.t * Expr.expr
  | RecFun of env * Var.t * Var.t * Expr.expr
  | Loc of Loc.t

and env = (Var.t * t) list

val to_string : t -> string

val env_to_string : env -> string
