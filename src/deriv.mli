type t

val output : ?indent:int -> ?outchan:out_channel -> t -> unit

exception Error of string * Expr.t

val eval : System.t -> Eval.ee -> Eval.ed * t
