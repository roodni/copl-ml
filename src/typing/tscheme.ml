type t = Tvar.t list * Types.t

let create (vlist : Tvar.t list) (ty : Types.t) = (vlist, ty)

let simple ty = create [] ty

let generate_instance (vlist, ty) =
  let sub =
    List.fold_left
      (fun sub v ->
        let b = Types.Var (Tvar.generate ()) in
        Tsub.add v b sub)
      Tsub.empty vlist
  in
  Tsub.substitute ty sub

let ftv (vlist, ty) = Tvset.diff (Types.ftv ty) (Tvset.of_list vlist)

let substitute sub (vlist, ty) = (vlist, Tsub.substitute ty sub)

let to_string (vlist, ty) =
  if vlist = [] then Types.to_string ty
  else
    (vlist |> List.map Tvar.to_string |> String.concat " ")
    ^ " . " ^ Types.to_string ty
