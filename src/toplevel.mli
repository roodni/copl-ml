type input_type = Judg | Eval

type t

val create : ?store:Store.t -> ?env:Value.env -> Expr.expr -> input_type -> t

val to_evalee : t -> Evalee.t

val is_judg : t -> bool
