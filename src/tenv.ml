type t = (Var.t * Tscheme.t) list

let ftv t =
  List.fold_left
    (fun set (_, sch) -> Tvset.union (Tscheme.ftv sch) set)
    Tvset.empty t

let substitute sub t =
  List.map (fun (v, sch) -> (v, Tscheme.substitute sub sch)) t

let rec to_string env =
  let bind_to_string (var, sch) =
    Printf.sprintf "%s : %s" (Var.to_string var) (Tscheme.to_string sch)
  in
  match env with
  | [] -> ""
  | [ bind ] -> bind_to_string bind
  | bind :: env' ->
      Printf.sprintf "%s, %s" (to_string env') (bind_to_string bind)
