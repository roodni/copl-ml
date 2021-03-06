open Printf

type t = Bool | Int | Fun of t * t | List of t | Var of Tvar.t

let rec ftv t =
  match t with
  | Int | Bool -> Tvset.empty
  | Fun (x, y) -> Tvset.union (ftv x) (ftv y)
  | List x -> ftv x
  | Var v -> Tvset.singleton v

let parenthesize s = sprintf "(%s)" s

let rec to_string = function
  | Bool -> "bool"
  | Int -> "int"
  | Fun (a, b) ->
      let a_s = to_string a in
      let a_s = match a with Fun (_, _) -> parenthesize a_s | _ -> a_s in
      sprintf "%s -> %s" a_s (to_string b)
  | List t ->
      let s = to_string t in
      let s = match t with Fun (_, _) -> parenthesize s | _ -> s in
      sprintf "%s list" s
  | Var t -> Tvar.to_string t
