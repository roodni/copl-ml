open Base
open Printf

module rec Value : sig
  type t =
    | Int of int
    | Bool of bool
    | Fun of Env.t * Var.t * Expr.t
    | RecFun of Env.t * Var.t * Var.t * Expr.t
    | Loc of Loc.t
    | Nil
    | Cons of t * t
    | Cont of Cont.t

  val to_string : t -> string
end = struct
  type t =
    | Int of int
    | Bool of bool
    | Fun of Env.t * Var.t * Expr.t
    | RecFun of Env.t * Var.t * Var.t * Expr.t
    | Loc of Loc.t
    | Nil
    | Cons of t * t
    | Cont of Cont.t

  let rec to_string = function
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Fun (env, v, expr) ->
        sprintf "(%s)[fun %s -> %s]" (Env.to_string env) (Var.to_string v)
          (Expr.to_string expr)
    | RecFun (env, f, a, expr) ->
        sprintf "(%s)[rec %s = fun %s -> %s]" (Env.to_string env)
          (Var.to_string f) (Var.to_string a) (Expr.to_string expr)
    | Loc l -> Loc.to_string l
    | Nil -> "[]"
    | Cons (l, r) ->
        let ls = to_string l in
        sprintf "%s :: %s"
          (match l with Cons _ -> "(" ^ ls ^ ")" | _ -> ls)
          (to_string r)
    | Cont k -> sprintf "[%s]" (Cont.to_string k)
end

and Env : sig
  type t = (Var.t * Value.t) list

  val to_string : t -> string
end = struct
  type t = (Var.t * Value.t) list

  let to_string env =
    let bind_to_string (var, value) =
      Printf.sprintf "%s = %s" (Var.to_string var) (Value.to_string value)
    in
    env |> List.rev_map bind_to_string |> String.concat ", "
end

and Cont : sig
  type u =
    | BOpL of Expr.binOp * Env.t option * Expr.t
    | BOpR of Expr.binOp * Value.t
    | If of Env.t option * Expr.t * Expr.t
    | Let of Env.t * Var.t * Expr.t
    | AppL of Env.t * Expr.t
    | AppR of Value.t
    | ConsL of Env.t * Expr.t
    | ConsR of Value.t
    | Match of Env.t * Expr.t * Var.t * Var.t * Expr.t

  type t = u list

  val to_string : t -> string
end = struct
  type u =
    | BOpL of Expr.binOp * Env.t option * Expr.t
    | BOpR of Expr.binOp * Value.t
    | If of Env.t option * Expr.t * Expr.t
    | Let of Env.t * Var.t * Expr.t
    | AppL of Env.t * Expr.t
    | AppR of Value.t
    | ConsL of Env.t * Expr.t
    | ConsR of Value.t
    | Match of Env.t * Expr.t * Var.t * Var.t * Expr.t

  let wild = Expr.Var (Var.of_string "_")

  let env_to_string = function
    | [] -> "|- "
    | env -> sprintf "%s |- " (Env.to_string env)

  let u_to_string = function
    | BOpL (op, env, e) ->
        let s_env =
          match env with None -> "" | Some env -> env_to_string env
        in
        sprintf "{%s%s}" s_env (Expr.BOp (op, wild, e) |> Expr.to_string)
    | BOpR (op, v) ->
        sprintf "{%s %s _}" (Value.to_string v) (Expr.binop_to_string op)
    | If (env, t, f) ->
        let s_env =
          match env with None -> "" | Some env -> env_to_string env
        in
        sprintf "{%s%s}" s_env (Expr.If (wild, t, f) |> Expr.to_string)
    | Let (env, x, e) ->
        sprintf "{%s%s}" (env_to_string env)
          (Expr.Let (x, wild, e) |> Expr.to_string)
    | AppL (env, e) ->
        sprintf "{%s%s}" (env_to_string env)
          (Expr.App (wild, e) |> Expr.to_string)
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
          ( Expr.Match
              (wild, [ (NilPat, e1); (ConsPat (VarPat x, VarPat y), e2) ])
          |> Expr.to_string )

  type t = u list

  let to_string = function
    | [] -> "_"
    | l -> l |> List.map u_to_string |> String.concat " >> "
end
