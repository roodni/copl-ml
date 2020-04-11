type value =
  | IntVal of int
  | BoolVal of bool
  | FunVal of env * Var.t * Expr.expr
  | RecFunVal of env * Var.t * Var.t * Expr.expr
  | LocVal of Loc.t

and env = (Var.t * value) list

val value_to_string : value -> string

val env_to_string : env -> string
