exception Typing_failed

exception Expr_error of string * Expr.t

module System : Deriv.SYSTEM

module TDeriv : module type of Deriv.Make (System)

val typing : Tenv.t -> Expr.t -> TDeriv.t
