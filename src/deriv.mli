type t

val output : ?indent:int -> ?outchan:out_channel -> t -> unit

exception EvalError of string

val eval : Evaluatee.t -> Value.value * t
