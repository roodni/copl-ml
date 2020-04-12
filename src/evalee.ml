type t = { store : Store.t; env : Value.env; expr : Expr.t }

let to_string { store; env; expr } =
  let s_store =
    if Store.is_empty store then "" else Store.to_string store ^ " / "
  and s_env = (if env <> [] then Value.env_to_string env ^ " " else "") ^ "|- "
  and s_expr = Expr.to_string expr in
  s_store ^ s_env ^ s_expr
