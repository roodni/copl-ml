type t = { env : Value.env; expr : Expr.expr }

let to_string { env; expr } =
  if env <> [] then
    Printf.sprintf "%s |- %s" (Value.env_to_string env)
      (Expr.expr_to_string expr)
  else Printf.sprintf "|- %s" (Expr.expr_to_string expr)
