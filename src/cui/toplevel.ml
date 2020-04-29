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

let create_eval ?store ?env expr ~is_judg = Eval { store; env; expr; is_judg }

let create_cont expr cont = Cont { expr; cont }
