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

let create_eval ?store ?env expr ~is_judg =
  Eval { store; env; expr; is_judg; cont = None }

let create_cont ?env expr cont =
  Eval { env; expr; cont = Some cont; store = None; is_judg = true }
