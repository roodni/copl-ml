type t = (Var.t * Tscheme.t) list

let ftv t =
  List.fold_left
    (fun set (_, sch) -> Tvset.union (Tscheme.ftv sch) set)
    Tvset.empty t

let substitute sub t =
  List.map (fun (v, sch) -> (v, Tscheme.substitute sub sch)) t

let to_string env =
  let bind_to_string (var, sch) =
    Printf.sprintf "%s : %s" (Var.to_string var) (Tscheme.to_string sch)
  in
  env |> List.rev_map bind_to_string |> String.concat ", "
