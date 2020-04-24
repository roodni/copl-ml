include Set.Make (Tvar)

let rec of_types = function
  | Types.Int | Types.Bool -> empty
  | Types.Fun (x, y) -> union (of_types x) (of_types y)
  | Types.List x -> of_types x
  | Types.Var v -> singleton v
