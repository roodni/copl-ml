open Base
open Table
open Printf

module System = struct
  type rule = string

  let rule_to_string (r : rule) = r

  type judgment =
    | EvalEJ of Env.t option * Expr.t * Cont.t * Value.t
    | EvalCJ of Value.t * Cont.t * Value.t
    | BinOpJ of Expr.binOp * int * int * Value.t

  let judgment_to_string = function
    | EvalEJ (env, e, k, v) ->
        let s_env =
          match env with
          | None -> ""
          | Some [] -> "|- "
          | Some env -> sprintf "%s |- " (Env.to_string env)
        in
        sprintf "%s%s >> %s evalto %s" s_env (Expr.to_string e)
          (Cont.to_string k) (Value.to_string v)
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
        in
        sprintf "%d %s %d is %s" l op_s r (Value.to_string v)
end

open System
module CDeriv = Deriv.Make (System)

let rec eval_expr env expr cont k =
  let return value rule premises =
    k (value, CDeriv.{ concl = EvalEJ (env, expr, cont, value); rule; premises })
  and bind_env k =
    match env with None -> Error "Missing environment" | Some env -> k env
  in
  match expr with
  | Expr.Int i ->
      eval_cont (Value.Int i) cont (fun (v, d) -> return v "E-Int" [ d ])
  | Expr.Bool b ->
      eval_cont (Value.Bool b) cont (fun (v, d) -> return v "E-Bool" [ d ])
  | Expr.BOp (op, e1, e2) ->
      eval_expr env e1
        (Cont.BOpL (op, env, e2) :: cont)
        (fun (v, d) -> return v "E-BinOp" [ d ])
  | Expr.If (c, t, f) ->
      eval_expr env c
        (Cont.If (env, t, f) :: cont)
        (fun (v, d) -> return v "E-If" [ d ])
  | Expr.Var v ->
      bind_env (fun env ->
          match List.assoc_opt v env with
          | None -> Error (sprintf "Undeclared variable: %s" (Var.to_string v))
          | Some v -> eval_cont v cont (fun (v, d) -> return v "E-Var" [ d ]))
  | Expr.Let (x, e1, e2) ->
      bind_env (fun env ->
          eval_expr (Some env) e1
            (Cont.Let (env, x, e2) :: cont)
            (fun (v, d) -> return v "E-Let" [ d ]))
  | Expr.Fun (x, e1) ->
      bind_env (fun env ->
          eval_cont
            (Value.Fun (env, x, e1))
            cont
            (fun (v, d) -> return v "E-Fun" [ d ]))
  | Expr.App (e1, e2) ->
      bind_env (fun env ->
          eval_expr (Some env) e1
            (Cont.AppL (env, e2) :: cont)
            (fun (v, d) -> return v "E-App" [ d ]))
  | Expr.LetRec (f, x, e1, e2) ->
      bind_env (fun env ->
          eval_expr
            (Some ((f, Value.RecFun (env, f, x, e1)) :: env))
            e2 cont
            (fun (v, d) -> return v "E-LetRec" [ d ]))
  | Expr.Nil -> eval_cont Value.Nil cont (fun (v, d) -> return v "E-Nil" [ d ])
  | Expr.Cons (e1, e2) ->
      bind_env (fun env ->
          eval_expr (Some env) e1
            (Cont.ConsL (env, e2) :: cont)
            (fun (v, d) -> return v "E-Cons" [ d ]))
  | Expr.Match (e1, [ (NilPat, e2); (ConsPat (VarPat x, VarPat y), e3) ]) ->
      bind_env (fun env ->
          eval_expr (Some env) e1
            (Cont.Match (env, e2, x, y, e3) :: cont)
            (fun (v, d) -> return v "E-Match" [ d ]))
  | Expr.Letcc (x, e) ->
      bind_env (fun env ->
          eval_expr
            (Some ((x, Value.Cont cont) :: env))
            e cont
            (fun (v, d) -> return v "E-LetCc" [ d ]))
  | _ -> Error (sprintf "Unimplemented expression: %s" (Expr.to_string expr))

and eval_cont value cont k =
  let return value' rule premises =
    k (value', CDeriv.{ concl = EvalCJ (value, cont, value'); rule; premises })
  in
  match cont with
  | [] -> return value "C-Ret" []
  | Cont.BOpL (op, env, e) :: cont ->
      eval_expr env e
        (Cont.BOpR (op, value) :: cont)
        (fun (v, d) -> return v "C-EvalR" [ d ])
  | Cont.BOpR (op, vleft) :: cont -> (
      match (vleft, value) with
      | Value.Int il, Value.Int ir ->
          let value, crule, brule =
            match op with
            | PlusOp -> (Value.Int (il + ir), "C-Plus", "B-Plus")
            | MinusOp -> (Value.Int (il - ir), "C-Minus", "B-Minus")
            | TimesOp -> (Value.Int (il * ir), "C-Times", "B-Times")
            | LtOp -> (Value.Bool (il < ir), "C-Lt", "B-Lt")
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
      | _ -> Error (sprintf "Type error: %s" (Expr.binop_to_string op)) )
  | Cont.If (env, t, f) :: cont -> (
      match value with
      | Value.Bool true ->
          eval_expr env t cont (fun (v, d) -> return v "C-IfT" [ d ])
      | Value.Bool false ->
          eval_expr env f cont (fun (v, d) -> return v "C-IfF" [ d ])
      | _ -> Error (sprintf "Type error: if") )
  | Cont.Let (env, x, e) :: cont ->
      eval_expr
        (Some ((x, value) :: env))
        e cont
        (fun (v, d) -> return v "C-LetBody" [ d ])
  | Cont.AppL (env, e) :: cont ->
      eval_expr (Some env) e (Cont.AppR value :: cont) (fun (v, d) ->
          return v "C-EvalArg" [ d ])
  | Cont.AppR vfun :: cont -> (
      match vfun with
      | Value.Fun (env, x, e) ->
          eval_expr
            (Some ((x, value) :: env))
            e cont
            (fun (v, d) -> return v "C-EvalFun" [ d ])
      | Value.RecFun (env, f, x, e) ->
          eval_expr
            (Some ((x, value) :: (f, vfun) :: env))
            e cont
            (fun (v, d) -> return v "C-EvalFunR" [ d ])
      | Value.Cont cont ->
          eval_cont value cont (fun (v, d) -> return v "C-EvalFunC" [ d ])
      | _ -> Error "Type error: EvalFun" )
  | Cont.ConsL (env, e) :: cont ->
      eval_expr (Some env) e (Cont.ConsR value :: cont) (fun (v, d) ->
          return v "C-EvalConsR" [ d ])
  | Cont.ConsR vleft :: cont ->
      eval_cont
        (Value.Cons (vleft, value))
        cont
        (fun (v, d) -> return v "C-Cons" [ d ])
  | Cont.Match (env, e1, x, y, e2) :: cont -> (
      match value with
      | Value.Nil ->
          eval_expr (Some env) e1 cont (fun (v, d) ->
              return v "C-MatchNil" [ d ])
      | Value.Cons (a, b) ->
          eval_expr
            (Some ((y, b) :: (x, a) :: env))
            e2 cont
            (fun (v, d) -> return v "C-MatchCons" [ d ])
      | _ -> Error "Type error: match" )
