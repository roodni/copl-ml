type ee = { store : Store.t; env : Value.env; expr : Expr.t }

let ee_to_string { store; env; expr } =
  let s_store =
    if Store.is_empty store then "" else Store.to_string store ^ " / "
  and s_env = (if env <> [] then Value.env_to_string env ^ " " else "") ^ "|- "
  and s_expr = Expr.to_string expr in
  s_store ^ s_env ^ s_expr

type ed = Value.t * Store.t

let ed_to_string (value, store) =
  Value.to_string value
  ^ if Store.is_empty store then "" else " / " ^ Store.to_string store

let ed_of_value value = (value, Store.empty)
