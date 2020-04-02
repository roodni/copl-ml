open Env
open Expr

type evaluatee = { env : env; expr : expr }

let evaluatee_to_string { env; expr } =
  if env <> [] then
    Printf.sprintf "%s |- %s" (env_to_string env) (expr_to_string expr)
  else Printf.sprintf "|- %s" (expr_to_string expr)
