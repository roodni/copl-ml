open Base

type t = ML1 | ML3 | RefML3 | ML4 | ML5

let to_string = function
  | ML1 -> "EvalML1"
  | ML3 -> "EvalML3"
  | RefML3 -> "EvalRefML3"
  | ML4 -> "EvalML4"
  | ML5 -> "EvalML5"

let parent = function
  | ML1 -> None
  | ML3 -> Some ML1
  | RefML3 -> Some ML3
  | ML4 -> Some ML3
  | ML5 -> Some ML4

let rec ( <<= ) a b =
  if a = b then true
  else match parent b with None -> false | Some p -> a <<= p

exception Error of t * t

let detect ?store ?env expr =
  let vmax e l =
    List.fold_left
      (fun v1 v2 ->
        if v1 <<= v2 then v2
        else if v2 <<= v1 then v1
        else raise @@ Error (v1, v2))
      e l
  in
  let rec expr_detect = function
    | Expr.Int _ | Expr.Bool _ -> ML1
    | Expr.BOp ((PlusOp | MinusOp | TimesOp | LtOp), l, r) ->
        vmax ML1 (List.map expr_detect [ l; r ])
    | Expr.If (c, t, f) -> vmax ML1 (List.map expr_detect [ c; t; f ])
    | Expr.Var _ -> ML3
    | Expr.Let (_, e1, e2) -> vmax ML3 (List.map expr_detect [ e1; e2 ])
    | Expr.Fun (_, e) -> vmax ML3 [ expr_detect e ]
    | Expr.App (e1, e2) -> vmax ML3 (List.map expr_detect [ e1; e2 ])
    | Expr.LetRec (_, _, e1, e2) -> vmax ML3 (List.map expr_detect [ e1; e2 ])
    | Expr.Ref e -> vmax RefML3 [ expr_detect e ]
    | Expr.Deref e -> vmax RefML3 [ expr_detect e ]
    | Expr.BOp (AssignOp, l, r) -> vmax RefML3 (List.map expr_detect [ l; r ])
    | Expr.Nil -> ML4
    | Expr.BOp (ConsOp, l, r) -> vmax ML4 (List.map expr_detect [ l; r ])
    | Expr.Match (e1, [ (NilPat, e2); (ConsPat (VarPat _, VarPat _), e3) ]) ->
        vmax ML4 (List.map expr_detect [ e1; e2; e3 ])
    | Expr.Match (_, []) -> failwith "Empty match clauses"
    | Expr.Match (e1, clauses) ->
        vmax ML5 (List.map expr_detect (e1 :: List.map snd clauses))
  in
  let ver =
    if store <> None then RefML3 else if env <> None then ML3 else ML1
  in
  vmax ver [ expr_detect expr ]
