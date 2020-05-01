open Base

module rec Value : sig
  type t =
    | Int of int
    | Bool of bool
    | Fun of Env.t * Var.t * Expr.t
    | RecFun of Env.t * Var.t * Var.t * Expr.t
    | Loc of Loc.t
    | Nil
    | Cons of t * t
    | Cont of Cont.t

  val to_string : t -> string
end

and Env : sig
  type t = (Var.t * Value.t) list

  val to_string : t -> string
end

and Cont : sig
  type u =
    | BOpL of Expr.binOp * Env.t option * Expr.t
    | BOpR of Expr.binOp * Value.t
    | If of Env.t option * Expr.t * Expr.t
    | Let of Env.t * Var.t * Expr.t
    | AppL of Env.t * Expr.t
    | AppR of Value.t
    | ConsL of Env.t * Expr.t
    | ConsR of Value.t
    | Match of Env.t * Expr.t * Var.t * Var.t * Expr.t

  type t = u list

  val to_string : t -> string
end
