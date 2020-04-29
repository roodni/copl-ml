open Base
open Printf

module System = struct
  type rule = string

  let rule_to_string (r : rule) = r

  type judgment =
    | EvalEJ of Expr.t * Cont.t * Value.t
    | EvalCJ of Value.t * Cont.t * Value.t
    | BinOpJ of Expr.binOp * int * int * Value.t

  let judgment_to_string = function
    | EvalEJ (e, k, v) ->
        sprintf "%s >> %s evalto %s" (Expr.to_string e) (Cont.to_string k)
          (Value.to_string v)
    | EvalCJ (v, k, v') ->
        sprintf "%s => %s evalto %s" (Value.to_string v) (Cont.to_string k)
          (Value.to_string v')
    | BinOpJ (op, l, r, v) ->
        let op_s =
          match op with
          | PlusOp -> "plus"
          | MinusOp -> "minus"
          | TimesOp -> "times"
          | LtOp -> "less than"
          | _ -> assert false
        in
        sprintf "%d %s %d is %s" l op_s r (Value.to_string v)
end

open System
module CDeriv = Deriv.Make (System)

exception Error of string

let rec eval_expr expr cont k =
  let return value rule premises =
    k (value, CDeriv.{ concl = EvalEJ (expr, cont, value); rule; premises })
  in
  match expr with
  | Expr.Int i ->
      eval_cont (Value.Int i) cont (fun (v, d) -> return v "E-Int" [ d ])
  | Expr.Bool b ->
      eval_cont (Value.Bool b) cont (fun (v, d) -> return v "E-Bool" [ d ])
  | Expr.BOp (op, e1, e2) ->
      eval_expr e1
        (Cont.BOpL (op, e2) :: cont)
        (fun (v, d) -> return v "E-BinOp" [ d ])
  | Expr.If (c, t, f) ->
      eval_expr c (Cont.If (t, f) :: cont) (fun (v, d) -> return v "E-If" [ d ])
  | _ -> raise @@ Error "hoge"

and eval_cont value cont k =
  let return value' rule premises =
    k (value', CDeriv.{ concl = EvalCJ (value, cont, value'); rule; premises })
  in
  match cont with
  | [] -> return value "C-Ret" []
  | Cont.BOpL (op, e) :: cont ->
      eval_expr e
        (Cont.BOpR (op, value) :: cont)
        (fun (v, d) -> return v "C-EvalR" [ d ])
  | Cont.BOpR (((PlusOp | MinusOp | TimesOp | LtOp) as op), vleft) :: cont -> (
      match (vleft, value) with
      | Value.Int il, Value.Int ir ->
          let value, crule, brule =
            match op with
            | PlusOp -> (Value.Int (il + ir), "C-Plus", "B-Plus")
            | MinusOp -> (Value.Int (il - ir), "C-Minus", "B-Minus")
            | TimesOp -> (Value.Int (il * ir), "C-Times", "B-Times")
            | LtOp -> (Value.Bool (il < ir), "C-Lt", "B-Lt")
            | _ -> assert false
          in
          let bderiv =
            CDeriv.
              {
                concl = BinOpJ (op, il, ir, value);
                rule = brule;
                premises = [];
              }
          in
          eval_cont value cont (fun (v, d) -> return v crule [ bderiv; d ])
      | _ -> raise @@ Error (sprintf "Type error: %s" (Expr.binop_to_string op))
      )
  | Cont.BOpR ((ConsOp | AssignOp), _) :: _ -> assert false
  | Cont.If (t, f) :: cont -> (
      match value with
      | Value.Bool true ->
          eval_expr t cont (fun (v, d) -> return v "C-IfT" [ d ])
      | Value.Bool false ->
          eval_expr f cont (fun (v, d) -> return v "C-IfF" [ d ])
      | _ -> raise @@ Error (sprintf "Type error: if") )
