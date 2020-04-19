type t = Bool | Int | Fun of t * t | List of t

val to_string : t -> string
