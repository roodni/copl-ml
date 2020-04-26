include Set.S with type elt = Types.t * Types.t

val of_tsub : Tsub.t -> t

val union_list : t list -> t

exception Unify_failed

val unify : t -> Tsub.t
