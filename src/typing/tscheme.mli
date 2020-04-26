type t

val create : Tvar.t list -> Types.t -> t

val simple : Types.t -> t

val generate_instance : t -> Types.t

val ftv : t -> Tvset.t

val substitute : Tsub.t -> t -> t

val to_string : t -> string
