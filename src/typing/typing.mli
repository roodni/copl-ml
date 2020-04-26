open Base

exception Typing_failed

exception Expr_error of string * Expr.t

module System : Deriv.SYSTEM

module TDeriv : module type of Deriv.Make (System)

val substitute_deriv : Tsub.t -> TDeriv.t -> TDeriv.t

val typing : poly:bool -> Tenv.t -> Expr.t -> Tsub.t * Types.t * TDeriv.t
