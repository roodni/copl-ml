open Expr

type t = { env : Env.t; expr : expr }

let to_string { env; expr } =
  if env <> [] then
    Printf.sprintf "%s |- %s" (Env.to_string env) (expr_to_string expr)
  else Printf.sprintf "|- %s" (expr_to_string expr)
