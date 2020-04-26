open Base

type t =
  | Eval of {
      store : Evalml.Store.t option;
      env : Evalml.Value.env option;
      expr : Expr.t;
      is_judg : bool;
    }
  | Typing of { tenv : Typingml.Tenv.t; expr : Expr.t }

let create_eval ?store ?env expr ~is_judg = Eval { store; env; expr; is_judg }
