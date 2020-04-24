include module type of Set.Make (struct
  type t = Types.t * Types.t

  let compare = compare
end)

val of_tsub : Tsub.t -> t

val union_list : t list -> t

exception Unify_failed

val unify : t -> Tsub.t
