module M = Map.Make (Tvar)
include M

type t = Types.t M.t

let rec substitute ty t =
  match ty with
  | Types.Bool -> Types.Bool
  | Types.Int -> Types.Int
  | Types.Fun (x, y) -> Types.Fun (substitute x t, substitute y t)
  | Types.List x -> Types.List (substitute x t)
  | Types.Var v -> (
      match find_opt v t with None -> Types.Var v | Some x -> x )

let composite t' t = fold (fun v ty sub' -> add v (substitute ty t') sub') t t'
