open Table

type t

exception Invalid_reference

val empty : t

val create : Loc.t list -> Value.t list -> t

val make_ref : t -> Value.t -> Loc.t * t

val assign : t -> Loc.t -> Value.t -> t

val deref : t -> Loc.t -> Value.t

val to_string : t -> string

val is_empty : t -> bool

val binds : t -> (Loc.t * Value.t) list
