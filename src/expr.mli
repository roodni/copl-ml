type binOp = PlusOp | MinusOp | TimesOp | LtOp

val binop_to_string : binOp -> string

type expr =
  | IntExp of int
  | BoolExp of bool
  | BOpExp of binOp * expr * expr
  | IfExp of expr * expr * expr
  | VarExp of Var.t
  | LetExp of Var.t * expr * expr
  | FunExp of Var.t * expr
  | AppExp of expr * expr
  | LetRecExp of Var.t * Var.t * expr * expr

val expr_to_string : expr -> string
