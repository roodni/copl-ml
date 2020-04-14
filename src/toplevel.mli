type input_type = Judg | Eval

type t

val create : ?store:Store.t -> ?env:Value.env -> Expr.t -> input_type -> t

val to_evalee : t -> Eval.ee

val is_judg : t -> bool
