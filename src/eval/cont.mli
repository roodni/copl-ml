open Base

type u =
  | BOpL of Expr.binOp * Value.env option * Expr.t
  | BOpR of Expr.binOp * Value.t
  | If of Value.env option * Expr.t * Expr.t
  | Let of Value.env * Var.t * Expr.t
  | AppL of Value.env * Expr.t
  | AppR of Value.t
  | ConsL of Value.env * Expr.t
  | ConsR of Value.t
  | Match of Value.env * Expr.t * Var.t * Var.t * Expr.t

type t = u list

val to_string : t -> string
