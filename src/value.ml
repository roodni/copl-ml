type value = IntVal of int | BoolVal of bool

let value_to_string = function
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
