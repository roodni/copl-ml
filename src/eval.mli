(* 評価されるもの *)
type ee = { store : Store.t; env : Value.env; expr : Expr.t }

val ee_of_toplevel : Toplevel.t -> ee

val ee_to_string : ee -> string

(* 評価されたもの *)
type ed = Value.t * Store.t

val ed_to_string : ed -> string

(* 評価導出システム *)
module EDeriv : sig
  type t

  val output : ?indent:int -> ?outchan:out_channel -> t -> unit
end

exception Error of string * Expr.t

val eval : Mlver.t -> ee -> ed * EDeriv.t
