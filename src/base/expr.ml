open Printf

type binOp = PlusOp | MinusOp | TimesOp | LtOp

let binop_to_string = function
  | PlusOp -> "+"
  | MinusOp -> "-"
  | TimesOp -> "*"
  | LtOp -> "<"

type pat = VarPat of Var.t | NilPat | ConsPat of pat * pat | WildPat

let rec pat_to_string = function
  | VarPat v -> Var.to_string v
  | NilPat -> "[]"
  | ConsPat (l, r) ->
      let lp = pat_to_string l in
      let rp = pat_to_string r in
      sprintf "%s :: %s"
        (match l with ConsPat _ -> "(" ^ lp ^ ")" | _ -> lp)
        rp
  | WildPat -> "_"

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
  | Assign of t * t
  | Nil
  | Cons of t * t
  | Match of t * (pat * t) list

let precedence = function
  | Match _ -> 10
  | Let _ | Fun _ | LetRec _ | If _ -> 20
  | Assign _ -> 30
  | BOp (LtOp, _, _) -> 40
  | Cons _ -> 50
  | BOp ((PlusOp | MinusOp), _, _) -> 60
  | BOp (TimesOp, _, _) -> 70
  | App _ | Ref _ -> 80
  | Deref _ -> 90
  | Int _ | Bool _ | Var _ | Nil -> 1000

let to_string expr =
  let rec to_string parent_prec expr =
    let prec = precedence expr in
    let s =
      match expr with
      | Int i -> string_of_int i
      | Bool b -> string_of_bool b
      | BOp (op, l, r) ->
          let lp, rp =
            match op with PlusOp | MinusOp | TimesOp | LtOp -> (prec - 1, prec)
          in
          sprintf "%s %s %s" (to_string lp l) (binop_to_string op)
            (to_string rp r)
      | If (c, t, f) ->
          sprintf "if %s then %s else %s" (to_string 0 c) (to_string 0 t)
            (to_string (prec - 1) f)
      | Var v -> Var.to_string v
      | Let (v, e1, e2) ->
          sprintf "let %s = %s in %s" (Var.to_string v) (to_string 0 e1)
            (to_string (prec - 1) e2)
      | Fun (v, e) ->
          sprintf "fun %s -> %s" (Var.to_string v) (to_string (prec - 1) e)
      | App (l, r) ->
          sprintf "%s %s" (to_string (prec - 1) l) (to_string prec r)
      | Ref e -> "ref " ^ to_string prec e
      | LetRec (f, x, e1, e2) ->
          sprintf "let rec %s = fun %s -> %s in %s" (Var.to_string f)
            (Var.to_string x) (to_string 0 e1)
            (to_string (prec - 1) e2)
      | Deref e -> "!" ^ to_string prec e
      | Assign (e1, e2) ->
          sprintf "%s := %s" (to_string prec e1) (to_string (prec - 1) e2)
      | Nil -> "[]"
      | Cons (e1, e2) ->
          sprintf "%s :: %s" (to_string prec e1) (to_string (prec - 1) e2)
      | Match (e, c) ->
          let rec clauses_to_string c =
            let m_to_string (pat, expr) =
              sprintf "%s -> %s" (pat_to_string pat) (to_string prec expr)
            in
            match c with
            | [ m ] -> m_to_string m
            | m :: rest ->
                sprintf "%s | %s" (m_to_string m) (clauses_to_string rest)
            | [] -> assert false
          in
          sprintf "match %s with %s" (to_string 0 e) (clauses_to_string c)
    in
    if prec > parent_prec then s else "(" ^ s ^ ")"
  in
  to_string 0 expr
