type t = { store : Store.t; env : Value.env; expr : Expr.t }

val to_string : t -> string
