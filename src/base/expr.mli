type binOp = PlusOp | MinusOp | TimesOp | LtOp | AssignOp | ConsOp

val binop_to_string : binOp -> string

type pat = VarPat of Var.t | NilPat | ConsPat of pat * pat | WildPat

val pat_to_string : pat -> string

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
  | Nil
  | Match of t * (pat * t) list

val to_string : t -> string
