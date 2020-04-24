type t

val empty : t

val singleton : Tvar.t -> Types.t -> t

val substitute : Types.t -> t -> Types.t

val substitute_env : Tenv.t -> t -> Tenv.t

val composite : t -> t -> t

val fold : (Tvar.t -> Types.t -> 'a -> 'a) -> t -> 'a -> 'a
