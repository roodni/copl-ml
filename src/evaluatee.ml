type t = { store : Store.t; env : Value.env; expr : Expr.expr }

let to_string { store; env; expr } =
  let s_store =
    if Store.is_empty store then "" else Store.to_string store ^ " / "
  and s_env = Value.env_to_string env ^ " |- "
  and s_expr = Expr.expr_to_string expr in
  s_store ^ s_env ^ s_expr
