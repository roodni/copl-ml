open Base

(* 評価されるもの *)
type ee

val ee_create : Mlver.t -> ?store:Store.t -> ?env:Value.env -> Expr.t -> ee

val ee_to_string : ee -> string

(* 評価されたもの *)
type ed = Value.t * Store.t

val ed_to_string : ed -> string

val ed_equal : ed -> ed -> bool

(* 評価導出システム *)
module EDeriv : sig
  type t

  val output : ?indent:int -> ?outchan:out_channel -> t -> unit
end

exception Error of string * Expr.t

val eval : ee -> ed * EDeriv.t
