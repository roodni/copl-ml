type binOp = PlusOp | MinusOp | TimesOp | LtOp

let binop_to_string = function
  | PlusOp -> "+"
  | MinusOp -> "-"
  | TimesOp -> "*"
  | LtOp -> "<"

type expr =
  | IntExp of int
  | BoolExp of bool
  | BOpExp of binOp * expr * expr
  | IfExp of expr * expr * expr
  | VarExp of Var.t
  | LetExp of Var.t * expr * expr

let rec expr_to_string = function
  | IntExp i -> string_of_int i
  | BoolExp b -> string_of_bool b
  | BOpExp (op, l, r) ->
      Printf.sprintf "(%s %s %s)" (expr_to_string l) (binop_to_string op)
        (expr_to_string r)
  | IfExp (c, t, f) ->
      Printf.sprintf "(if %s then %s else %s)" (expr_to_string c)
        (expr_to_string t) (expr_to_string f)
  | VarExp v -> Var.to_string v
  | LetExp (v, e1, e2) ->
      Printf.sprintf "(let %s = %s in %s)" (Var.to_string v) (expr_to_string e1)
        (expr_to_string e2)
