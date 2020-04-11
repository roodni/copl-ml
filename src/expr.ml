open Printf

type binOp = PlusOp | MinusOp | TimesOp | LtOp | AssignOp

let binop_to_string = function
  | PlusOp -> "+"
  | MinusOp -> "-"
  | TimesOp -> "*"
  | LtOp -> "<"
  | AssignOp -> ":="

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
  | RefExp of expr
  | DerefExp of expr

let precedence = function
  | LetExp _ | FunExp _ | LetRecExp _ | IfExp _ -> 10
  | BOpExp (AssignOp, _, _) -> 20
  | BOpExp (LtOp, _, _) -> 30
  | BOpExp ((PlusOp | MinusOp), _, _) -> 40
  | BOpExp (TimesOp, _, _) -> 50
  | AppExp _ | RefExp _ -> 60
  | DerefExp _ -> 70
  | IntExp _ | BoolExp _ | VarExp _ -> 80

let expr_to_string expr =
  let rec conv parent_prec expr =
    let prec = precedence expr in
    let s =
      match expr with
      | IntExp i -> string_of_int i
      | BoolExp b -> string_of_bool b
      | BOpExp (op, l, r) ->
          let lp, rp =
            match op with
            | PlusOp | MinusOp | TimesOp | LtOp -> (prec - 1, prec)
            | AssignOp -> (prec, prec - 1)
          in
          sprintf "%s %s %s" (conv lp l) (binop_to_string op) (conv rp r)
      | IfExp (c, t, f) ->
          sprintf "if %s then %s else %s" (conv 0 c) (conv 0 t) (conv 0 f)
      | VarExp v -> Var.to_string v
      | LetExp (v, e1, e2) ->
          sprintf "let %s = %s in %s" (Var.to_string v) (conv 0 e1) (conv 0 e2)
      | FunExp (v, e) -> sprintf "fun %s -> %s" (Var.to_string v) (conv 0 e)
      | AppExp (l, r) -> sprintf "%s %s" (conv (prec - 1) l) (conv prec r)
      | RefExp e -> "ref " ^ conv prec e
      | LetRecExp (f, x, e1, e2) ->
          sprintf "let rec %s = fun %s -> %s in %s" (Var.to_string f)
            (Var.to_string x) (conv 0 e1) (conv 0 e2)
      | DerefExp e -> "!" ^ conv prec e
    in
    if prec > parent_prec then s else "(" ^ s ^ ")"
  in
  conv 0 expr
