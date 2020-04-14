type t

val output : ?indent:int -> ?outchan:out_channel -> t -> unit

exception Error of string * Expr.t

val eval : Mlver.t -> Eval.ee -> Eval.ed * t
