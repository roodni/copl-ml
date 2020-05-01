open Base
open Printf

type u =
  | BOpL of Expr.binOp * Value.env option * Expr.t
  | BOpR of Expr.binOp * Value.t
  | If of Value.env option * Expr.t * Expr.t
  | Let of Value.env * Var.t * Expr.t
  | AppL of Value.env * Expr.t
  | AppR of Value.t
  | ConsL of Value.env * Expr.t
  | ConsR of Value.t
  | Match of Value.env * Expr.t * Var.t * Var.t * Expr.t

let wild = Expr.Var (Var.of_string "_")

let env_to_string = function
  | [] -> "|- "
  | env -> sprintf "%s |- " (Value.env_to_string env)

let u_to_string = function
  | BOpL (op, env, e) ->
      let s_env = match env with None -> "" | Some env -> env_to_string env in
      sprintf "{%s%s}" s_env (Expr.BOp (op, wild, e) |> Expr.to_string)
  | BOpR (op, v) ->
      sprintf "{%s %s _}" (Value.to_string v) (Expr.binop_to_string op)
  | If (env, t, f) ->
      let s_env = match env with None -> "" | Some env -> env_to_string env in
      sprintf "{%s%s}" s_env (Expr.If (wild, t, f) |> Expr.to_string)
  | Let (env, x, e) ->
      sprintf "{%s%s}" (env_to_string env)
        (Expr.Let (x, wild, e) |> Expr.to_string)
  | AppL (env, e) ->
      sprintf "{%s%s}" (env_to_string env) (Expr.App (wild, e) |> Expr.to_string)
  | AppR v -> sprintf "{%s _}" (Value.to_string v)
  | ConsL (env, e) ->
      sprintf "{%s%s}" (env_to_string env)
        (Expr.Cons (wild, e) |> Expr.to_string)
  | ConsR v ->
      let s =
        match v with
        | Value.Cons _ -> sprintf "(%s)" (Value.to_string v)
        | _ -> Value.to_string v
      in
      sprintf "{%s :: _}" s
  | Match (env, e1, x, y, e2) ->
      sprintf "{%s%s}" (env_to_string env)
        ( Expr.Match (wild, [ (NilPat, e1); (ConsPat (VarPat x, VarPat y), e2) ])
        |> Expr.to_string )

type t = u list

let to_string = function
  | [] -> "_"
  | l -> l |> List.map u_to_string |> String.concat " >> "
