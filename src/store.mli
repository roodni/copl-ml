type t

exception Invalid_reference

val empty : t

val create : Loc.t list -> Value.value list -> t

val make_ref : t -> Value.value -> Loc.t * t

val assign : t -> Loc.t -> Value.value -> t

val deref : t -> Loc.t -> Value.value

val to_string : t -> string

val is_empty : t -> bool

val binds : t -> (Loc.t * Value.value) list
