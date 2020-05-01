open Base

type t =
  | Eval of {
      store : Evalml.Store.t option;
      env : Evalml.Table.Env.t option;
      expr : Expr.t;
      cont : Evalml.Table.Cont.t option;
      is_judg : bool;
    }
  | Typing of { tenv : Typingml.Tenv.t; expr : Expr.t }

val create_eval :
  ?store:Evalml.Store.t ->
  ?env:Evalml.Table.Env.t ->
  Expr.t ->
  is_judg:bool ->
  t

val create_cont : ?env:Evalml.Table.Env.t -> Expr.t -> Evalml.Table.Cont.t -> t
