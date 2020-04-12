open Printf

type binOp = PlusOp | MinusOp | TimesOp | LtOp | AssignOp

let binop_to_string = function
  | PlusOp -> "+"
  | MinusOp -> "-"
  | TimesOp -> "*"
  | LtOp -> "<"
  | AssignOp -> ":="

type t =
  | Int of int
  | Bool of bool
  | BOp of binOp * t * t
  | If of t * t * t
  | Var of Var.t
  | Let of Var.t * t * t
  | Fun of Var.t * t
  | App of t * t
  | LetRec of Var.t * Var.t * t * t
  | Ref of t
  | Deref of t

let precedence = function
  | Let _ | Fun _ | LetRec _ | If _ -> 10
  | BOp (AssignOp, _, _) -> 20
  | BOp (LtOp, _, _) -> 30
  | BOp ((PlusOp | MinusOp), _, _) -> 40
  | BOp (TimesOp, _, _) -> 50
  | App _ | Ref _ -> 60
  | Deref _ -> 70
  | Int _ | Bool _ | Var _ -> 80

let to_string expr =
  let rec conv parent_prec expr =
    let prec = precedence expr in
    let s =
      match expr with
      | Int i -> string_of_int i
      | Bool b -> string_of_bool b
      | BOp (op, l, r) ->
          let lp, rp =
            match op with
            | PlusOp | MinusOp | TimesOp | LtOp -> (prec - 1, prec)
            | AssignOp -> (prec, prec - 1)
          in
          sprintf "%s %s %s" (conv lp l) (binop_to_string op) (conv rp r)
      | If (c, t, f) ->
          sprintf "if %s then %s else %s" (conv 0 c) (conv 0 t) (conv 0 f)
      | Var v -> Var.to_string v
      | Let (v, e1, e2) ->
          sprintf "let %s = %s in %s" (Var.to_string v) (conv 0 e1) (conv 0 e2)
      | Fun (v, e) -> sprintf "fun %s -> %s" (Var.to_string v) (conv 0 e)
      | App (l, r) -> sprintf "%s %s" (conv (prec - 1) l) (conv prec r)
      | Ref e -> "ref " ^ conv prec e
      | LetRec (f, x, e1, e2) ->
          sprintf "let rec %s = fun %s -> %s in %s" (Var.to_string f)
            (Var.to_string x) (conv 0 e1) (conv 0 e2)
      | Deref e -> "!" ^ conv prec e
    in
    if prec > parent_prec then s else "(" ^ s ^ ")"
  in
  conv 0 expr
