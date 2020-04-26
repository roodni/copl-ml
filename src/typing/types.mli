type t = Bool | Int | Fun of t * t | List of t | Var of Tvar.t

val ftv : t -> Tvset.t

val to_string : t -> string
