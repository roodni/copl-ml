type t

val output : ?indent:int -> ?outchan:out_channel -> t -> unit

exception EvalError of string

val eval :
  ?single_step_var:bool -> ?use_mult:bool -> Evaluatee.t -> Evaluated.t * t
