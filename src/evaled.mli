type t = Value.value * Store.t

val to_string : t -> string

val of_value : Value.value -> t

val value : t -> Value.value

val store : t -> Store.t
