open Base

type t = (Var.t * Tscheme.t) list

val ftv : t -> Tvset.t

val substitute : Tsub.t -> t -> t

val to_string : t -> string
