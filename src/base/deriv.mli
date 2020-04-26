module type SYSTEM = sig
  type rule

  type judgment

  val rule_to_string : rule -> string

  val judgment_to_string : judgment -> string
end

module Make (Sy : SYSTEM) : sig
  type t = { concl : Sy.judgment; rule : Sy.rule; premises : t list }

  val output : ?indent:int -> ?outchan:out_channel -> t -> unit
end
