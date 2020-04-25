open Printf

type t = Generated of int | Named of string

let varnum = ref 1

let generate () =
  let v = !varnum in
  incr varnum;
  Generated v

let of_name s = Named s

let is_named = function Generated _ -> false | Named _ -> true

let to_string = function
  | Generated i -> sprintf "'_a%d" i
  | Named s -> sprintf "'%s" s

let compare a b = compare a b
