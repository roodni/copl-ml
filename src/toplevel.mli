type t =
  | Eval of {
      store : Store.t option;
      env : Value.env option;
      expr : Expr.t;
      is_judg : bool;
    }
  | Typing of { tenv : Tenv.t; expr : Expr.t }

val create_eval :
  ?store:Store.t -> ?env:Value.env -> Expr.t -> is_judg:bool -> t
