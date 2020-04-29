open Base

type u =
  | BOpL of Expr.binOp * Expr.t
  | BOpR of Expr.binOp * Value.t
  | If of Expr.t * Expr.t

type t = u list

val to_string : t -> string
