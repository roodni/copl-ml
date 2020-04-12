type t = Value.t * Store.t

val to_string : t -> string

val of_value : Value.t -> t

val value : t -> Value.t

val store : t -> Store.t
