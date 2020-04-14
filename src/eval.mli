(* 評価されるもの *)
type ee = { store : Store.t; env : Value.env; expr : Expr.t }

val ee_to_string : ee -> string

(* 評価されたもの *)
type ed = Value.t * Store.t

val ed_to_string : ed -> string

val ed_of_value : Value.t -> ed
