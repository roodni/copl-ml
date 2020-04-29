open Base

type t =
  | Eval of {
      store : Evalml.Store.t option;
      env : Evalml.Value.env option;
      expr : Expr.t;
      is_judg : bool;
    }
  | Typing of { tenv : Typingml.Tenv.t; expr : Expr.t }
  | Cont of { expr : Expr.t; cont : Evalml.Cont.t }

val create_eval :
  ?store:Evalml.Store.t -> ?env:Evalml.Value.env -> Expr.t -> is_judg:bool -> t

val create_cont : Expr.t -> Evalml.Cont.t -> t
