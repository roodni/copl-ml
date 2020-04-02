open Env
open Exp

type evaluatee = { env : env; exp : exp }

let evaluatee_to_string { env; exp } =
  if env <> [] then
    Printf.sprintf "%s |- %s" (env_to_string env) (exp_to_string exp)
  else Printf.sprintf "|- %s" (exp_to_string exp)
