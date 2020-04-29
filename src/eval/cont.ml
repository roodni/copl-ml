open Base
open Printf

type u =
  | BOpL of Expr.binOp * Expr.t
  | BOpR of Expr.binOp * Value.t
  | If of Expr.t * Expr.t

let u_to_string = function
  | BOpL (op, e) ->
      sprintf "{%s}"
        (Expr.BOp (op, Expr.Var (Var.of_string "_"), e) |> Expr.to_string)
  | BOpR (op, v) ->
      sprintf "{%s %s _}" (Value.to_string v) (Expr.binop_to_string op)
  | If (t, f) ->
      sprintf "{if _ then %s else %s}" (Expr.to_string t) (Expr.to_string f)

type t = u list

let to_string = function
  | [] -> "_"
  | l -> l |> List.map u_to_string |> String.concat " >> "
