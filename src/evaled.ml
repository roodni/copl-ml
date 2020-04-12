type t = Value.t * Store.t

let to_string (value, store) =
  Value.to_string value
  ^ if Store.is_empty store then "" else " / " ^ Store.to_string store

let of_value value = (value, Store.empty)

let value t = fst t

let store t = snd t
