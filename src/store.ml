open Printf

type t = {
  pooled_locs : Loc.t list;
  name_count : int;
  binds : (Loc.t * Value.t) list;
}

exception Invalid_reference

let empty = { pooled_locs = []; name_count = 1; binds = [] }

let rec make_ref t v =
  match t.pooled_locs with
  | l :: rest -> (l, { t with pooled_locs = rest; binds = (l, v) :: t.binds })
  | [] ->
      let l = Loc.of_string (sprintf "l%d" t.name_count) in
      if List.exists (fun (l', _) -> l' = l) t.binds then
        make_ref { t with name_count = t.name_count + 1 } v
      else
        (l, { t with name_count = t.name_count + 1; binds = (l, v) :: t.binds })

let create locs values =
  let t = { pooled_locs = locs; name_count = 1; binds = [] } in
  List.fold_left (fun t v -> make_ref t v |> snd) t values

let assign t loc value =
  let rec f = function
    | (loc', _) :: rest when loc' = loc -> (loc, value) :: rest
    | bind :: rest -> bind :: f rest
    | [] -> raise Invalid_reference
  in
  { t with binds = f t.binds }

let deref t loc =
  try List.assoc loc t.binds with Not_found -> raise Invalid_reference

let to_string t =
  let bind_to_string (loc, value) =
    sprintf "%s = %s" (Loc.to_string loc) (Value.to_string value)
  in
  let rec binds_to_string = function
    | [] -> ""
    | [ bind ] -> bind_to_string bind
    | bind :: rest ->
        sprintf "%s, %s" (binds_to_string rest) (bind_to_string bind)
  in
  binds_to_string t.binds

let is_empty { binds; _ } = binds = []

let binds t = t.binds
