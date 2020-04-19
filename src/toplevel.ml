type t =
  | Eval of {
      store : Store.t option;
      env : Value.env option;
      expr : Expr.t;
      is_judg : bool;
    }
  | Typing of { tenv : Tenv.t; expr : Expr.t }

let create_eval ?store ?env expr ~is_judg = Eval { store; env; expr; is_judg }
