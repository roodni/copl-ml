include Set.Make (struct
  type t = Types.t * Types.t

  let compare = compare
end)

let of_tsub sub =
  Tsub.fold (fun v ty eqs -> add (Types.Var v, ty) eqs) sub empty

let union_list l = List.fold_left (fun t1 t2 -> union t1 t2) empty l

exception Unify_failed

let rec unify eqs =
  match choose_opt eqs with
  | None -> Tsub.empty
  | Some (ty1, ty2) -> (
      let eqs = remove (ty1, ty2) eqs in
      match (ty1, ty2) with
      | x, y when x = y -> unify eqs
      | Types.Var v, ty | ty, Types.Var v ->
          if Ftv.mem v (Ftv.of_types ty) then raise Unify_failed
          else
            let sub = Tsub.singleton v ty in
            let s =
              unify
                (map
                   (fun (ty1, ty2) ->
                     (Tsub.substitute ty1 sub, Tsub.substitute ty2 sub))
                   eqs)
            in
            Tsub.composite s sub
      | Types.Fun (ty11, ty12), Types.Fun (ty21, ty22) ->
          unify (union eqs (of_list [ (ty11, ty21); (ty12, ty22) ]))
      | Types.List ty1, Types.List ty2 -> unify (add (ty1, ty2) eqs)
      | _ -> raise Unify_failed )
