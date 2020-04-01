type binOp = PlusOp | MinusOp | TimesOp | LtOp

let binop_to_string = function
  | PlusOp -> "+"
  | MinusOp -> "-"
  | TimesOp -> "*"
  | LtOp -> "<"

type exp =
  | IntExp of int
  | BoolExp of bool
  | BOpExp of binOp * exp * exp
  | IfExp of exp * exp * exp

let rec exp_to_string = function
  | IntExp i -> string_of_int i
  | BoolExp b -> string_of_bool b
  | BOpExp (op, l, r) ->
      Printf.sprintf "%s %s %s" (exp_to_string l) (binop_to_string op)
        (exp_to_string r)
  | IfExp (c, t, f) ->
      Printf.sprintf "if %s then %s else %s" (exp_to_string c) (exp_to_string t)
        (exp_to_string f)

let rec exp_to_s_string = function
  | IntExp i -> string_of_int i
  | BoolExp b -> string_of_bool b
  | BOpExp (op, l, r) ->
      Printf.sprintf "(%s %s %s)" (binop_to_string op) (exp_to_s_string l)
        (exp_to_s_string r)
  | IfExp (c, t, f) ->
      Printf.sprintf "(if %s %s %s)" (exp_to_s_string c) (exp_to_s_string t)
        (exp_to_s_string f)
