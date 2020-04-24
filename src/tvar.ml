open Printf

type t = int

let varnum = ref 0

let create () =
  let v = !varnum in
  incr varnum;
  v

let to_string t = sprintf "'a%d" t

let compare a b = compare a b
