type t = { store : Store.t; env : Value.env; expr : Expr.expr }

val to_string : t -> string
