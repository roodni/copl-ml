module M = Map.Make (Tvar)

type t = Types.t M.t

let empty = M.empty

let singleton v ty = M.singleton v ty

let rec substitute ty t =
  match ty with
  | Types.Bool -> Types.Bool
  | Types.Int -> Types.Int
  | Types.Fun (x, y) -> Types.Fun (substitute x t, substitute y t)
  | Types.List x -> Types.List (substitute x t)
  | Types.Var v -> (
      match M.find_opt v t with None -> Types.Var v | Some x -> x )

let substitute_env env t = List.map (fun (v, ty) -> (v, substitute ty t)) env

let composite t' t =
  M.fold (fun v ty sub' -> M.add v (substitute ty t') sub') t t'

let fold f t a = M.fold f t a
