type t

val generate : unit -> t

val of_name : string -> t

val is_named : t -> bool

val to_string : t -> string

val compare : t -> t -> int
