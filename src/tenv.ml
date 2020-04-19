type t = (Var.t * Types.t) list

let rec to_string env =
  let bind_to_string (var, ty) =
    Printf.sprintf "%s : %s" (Var.to_string var) (Types.to_string ty)
  in
  match env with
  | [] -> ""
  | [ bind ] -> bind_to_string bind
  | bind :: env' ->
      Printf.sprintf "%s, %s" (to_string env') (bind_to_string bind)
