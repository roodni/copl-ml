type t = Value.value * Store.t

let to_string (value, store) =
  Value.value_to_string value
  ^ if Store.is_empty store then "" else " / " ^ Store.to_string store

let of_value value = (value, Store.empty)

let to_value t = fst t
