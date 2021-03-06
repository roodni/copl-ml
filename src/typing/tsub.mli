type t

val empty : t

val composite : t -> t -> t

val fold : (Tvar.t -> Types.t -> 'a -> 'a) -> t -> 'a -> 'a

val add : Tvar.t -> Types.t -> t -> t

val singleton : Tvar.t -> Types.t -> t

val substitute : Types.t -> t -> Types.t
