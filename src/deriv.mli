type t

val output : ?indent:int -> ?outchan:out_channel -> t -> unit

exception EvalError of string

val eval : System.t -> Evaluatee.t -> Evaluated.t * t
