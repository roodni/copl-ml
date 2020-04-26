open Printf

type ee = { store : Store.t; env : Value.env; expr : Expr.t; mlver : Mlver.t }

let ee_create mlver ?(store = Store.empty) ?(env = []) expr =
  { store; env; expr; mlver }

let ee_to_string { store; env; expr; mlver } =
  let s_store =
    if Store.is_empty store then "" else Store.to_string store ^ " / "
  and s_env =
    if mlver = Mlver.ML1 then ""
    else if env = [] then "|- "
    else sprintf "%s |- " (Value.env_to_string env)
  and s_expr = Expr.to_string expr in
  s_store ^ s_env ^ s_expr

type ed = Value.t * Store.t

let ed_to_string (value, store) =
  Value.to_string value
  ^ if Store.is_empty store then "" else " / " ^ Store.to_string store

let ed_of_value value = (value, Store.empty)

let ed_equal (v1, s1) (v2, s2) = (v1, Store.binds s1) = (v2, Store.binds s2)

module System = struct
  type rule =
    | EInt
    | EBool
    | EIfT
    | EIfF
    | EPlus
    | EMinus
    | ETimes
    | ELt
    | BPlus
    | BMinus
    | BTimes
    | BLt
    | EVar1
    | EVar2
    | ELet
    | EFun
    | EApp
    | ELetRec
    | EAppRec
    | EVar
    | EMult
    | BMult
    | EAssign
    | ERef
    | EDeref
    | ENil
    | ECons
    | EMatchNil
    | EMatchCons
    | MVar
    | MNil
    | MCons
    | MWild
    | NMConsNil
    | NMNilCons
    | NMConsConsL
    | NMConsConsR
    | EMatchM1
    | EMatchM2
    | EMatchN

  let rule_to_string = function
    | EInt -> "E-Int"
    | EBool -> "E-Bool"
    | EIfT -> "E-IfT"
    | EIfF -> "E-IfF"
    | EPlus -> "E-Plus"
    | EMinus -> "E-Minus"
    | ETimes -> "E-Times"
    | ELt -> "E-Lt"
    | BPlus -> "B-Plus"
    | BMinus -> "B-Minus"
    | BTimes -> "B-Times"
    | BLt -> "B-Lt"
    | EVar1 -> "E-Var1"
    | EVar2 -> "E-Var2"
    | ELet -> "E-Let"
    | EFun -> "E-Fun"
    | EApp -> "E-App"
    | ELetRec -> "E-LetRec"
    | EAppRec -> "E-AppRec"
    | EVar -> "E-Var"
    | EMult -> "E-Mult"
    | BMult -> "B-Mult"
    | EAssign -> "E-Assign"
    | ERef -> "E-Ref"
    | EDeref -> "E-Deref"
    | ENil -> "E-Nil"
    | ECons -> "E-Cons"
    | EMatchNil -> "E-MatchNil"
    | EMatchCons -> "E-MatchCons"
    | MVar -> "M-Var"
    | MNil -> "M-Nil"
    | MCons -> "M-Cons"
    | MWild -> "M-Wild"
    | NMConsNil -> "NM-ConsNil"
    | NMNilCons -> "NM-NilCons"
    | NMConsConsL -> "NM-ConsConsL"
    | NMConsConsR -> "NM-ConsConsR"
    | EMatchM1 -> "E-MatchM1"
    | EMatchM2 -> "E-MatchM2"
    | EMatchN -> "E-MatchN"

  type matched = Value.env option

  type judgment =
    | EvalJ of { evalee : ee; evaled : ed }
    | PlusJ of int * int * int
    | MinusJ of int * int * int
    | TimesJ of int * int * int
    | LtJ of int * int * bool
    | MatchJ of Expr.pat * Value.t * matched

  let judgment_to_string = function
    | EvalJ { evalee; evaled } ->
        sprintf "%s evalto %s" (ee_to_string evalee) (ed_to_string evaled)
    | PlusJ (l, r, s) -> sprintf "%d plus %d is %d" l r s
    | MinusJ (l, r, d) -> sprintf "%d minus %d is %d" l r d
    | TimesJ (l, r, p) -> sprintf "%d times %d is %d" l r p
    | LtJ (l, r, b) -> sprintf "%d less than %d is %b" l r b
    | MatchJ (p, v, m) -> (
        match m with
        | Some e ->
            sprintf "%s matches %s when (%s)" (Expr.pat_to_string p)
              (Value.to_string v) (Value.env_to_string e)
        | None ->
            sprintf "%s doesn't match %s" (Expr.pat_to_string p)
              (Value.to_string v) )
end

module EDeriv = Deriv.Make (System)

exception Error of string * Expr.t

let eval evalee =
  let open System in
  let rec eval ?ml5_match_root evalee =
    let { store; env; expr; mlver } = evalee in
    let ee_create ?(store = store) ?(env = env) expr =
      { store; env; expr; mlver }
    in
    let error s = raise @@ Error (s, expr) in
    let evaled, rule, premises =
      match expr with
      | Expr.Int i -> ((Value.Int i, store), EInt, [])
      | Expr.Bool b -> ((Value.Bool b, store), EBool, [])
      | Expr.If (c, t, f) ->
          let (cvalue, store), cderiv = eval @@ ee_create c in
          let retexpr, rule =
            match cvalue with
            | Value.Bool true -> (t, EIfT)
            | Value.Bool false -> (f, EIfF)
            | _ -> error "Condition must be bool"
          in
          let ret, retderiv = eval @@ ee_create ~store retexpr in
          (ret, rule, [ cderiv; retderiv ])
      | Expr.BOp (((PlusOp | MinusOp | TimesOp | LtOp) as op), lexpr, rexpr)
        -> (
          let (lvalue, store), lderiv = eval @@ ee_create lexpr in
          let (rvalue, store), rderiv = eval @@ ee_create ~store rexpr in
          match (lvalue, rvalue) with
          | Value.Int li, Value.Int ri ->
              let value, erule, bjudg, brule =
                match op with
                | PlusOp ->
                    let i = li + ri in
                    (Value.Int i, EPlus, PlusJ (li, ri, i), BPlus)
                | MinusOp ->
                    let i = li - ri in
                    (Value.Int i, EMinus, MinusJ (li, ri, i), BMinus)
                | TimesOp ->
                    let i = li * ri in
                    let erule, brule =
                      match mlver with
                      | Mlver.RefML3 -> (EMult, BMult)
                      | _ -> (ETimes, BTimes)
                    in
                    (Value.Int i, erule, TimesJ (li, ri, i), brule)
                | LtOp ->
                    let b = li < ri in
                    (Value.Bool b, ELt, LtJ (li, ri, b), BLt)
                | _ -> assert false
              in
              ( (value, store),
                erule,
                [
                  lderiv;
                  rderiv;
                  EDeriv.{ concl = bjudg; rule = brule; premises = [] };
                ] )
          | _ -> error "Both arguments must be int" )
      | Expr.BOp (AssignOp, lexpr, rexpr) -> (
          let (lvalue, store), lderiv = eval @@ ee_create lexpr in
          let (rvalue, store), rderiv = eval @@ ee_create ~store rexpr in
          match lvalue with
          | Value.Loc loc ->
              let store =
                try Store.assign store loc rvalue
                with Store.Invalid_reference -> error "Invalid reference"
              in
              ((rvalue, store), EAssign, [ lderiv; rderiv ])
          | _ -> error (sprintf "%s is not loc" (Expr.to_string lexpr)) )
      | Expr.Var v -> (
          match mlver with
          | ML1 | ML3 -> (
              match env with
              | (v', value) :: _ when v = v' -> (ed_of_value value, EVar1, [])
              | (_, _) :: tail ->
                  let evaled, premise = eval @@ ee_create ~env:tail expr in
                  (evaled, EVar2, [ premise ])
              | [] -> error "Undeclared variable" )
          | RefML3 | ML4 | ML5 ->
              (* 1 step var *)
              let value =
                try List.assoc v env
                with Not_found -> error "Undeclared variable"
              in
              ((value, store), EVar, []) )
      | Expr.Let (v, e1, e2) ->
          let (value1, store), deriv1 = eval @@ ee_create e1 in
          let (value2, store), deriv2 =
            eval @@ ee_create ~store ~env:((v, value1) :: env) e2
          in
          ((value2, store), ELet, [ deriv1; deriv2 ])
      | Expr.Fun (v, e) -> ((Value.Fun (env, v, e), store), EFun, [])
      | Expr.App (e1, e2) -> (
          let (fval, store), fderiv = eval @@ ee_create e1 in
          let (aval, store), aderiv = eval @@ ee_create ~store e2 in
          match fval with
          | Value.Fun (fenv, avar, fexpr) ->
              let evaled, deriv =
                eval @@ ee_create ~store ~env:((avar, aval) :: fenv) fexpr
              in
              (evaled, EApp, [ fderiv; aderiv; deriv ])
          | Value.RecFun (fenv, fvar, avar, fexpr) ->
              let evaled, deriv =
                eval
                @@ ee_create ~store
                     ~env:((avar, aval) :: (fvar, fval) :: fenv)
                     fexpr
              in

              (evaled, EAppRec, [ fderiv; aderiv; deriv ])
          | _ -> error (sprintf "%s cannot be applied" (Expr.to_string e1)) )
      | Expr.LetRec (f, a, e1, e2) ->
          let evaled, premise =
            eval @@ ee_create ~env:((f, Value.RecFun (env, f, a, e1)) :: env) e2
          in
          (evaled, ELetRec, [ premise ])
      | Expr.Ref e ->
          let (value, store), premise = eval @@ ee_create e in
          let loc, store = Store.make_ref store value in
          ((Value.Loc loc, store), ERef, [ premise ])
      | Expr.Deref e -> (
          let (value, store), premise = eval @@ ee_create e in
          match value with
          | Value.Loc loc ->
              let value =
                try Store.deref store loc
                with Store.Invalid_reference -> error "Invalid reference"
              in
              ((value, store), EDeref, [ premise ])
          | _ -> error (sprintf "%s must be loc" (Expr.to_string e)) )
      | Expr.Nil -> (ed_of_value Value.Nil, ENil, [])
      | Expr.BOp (ConsOp, l, r) ->
          let (lvalue, _), lderiv = eval @@ ee_create l in
          let (rvalue, _), rderiv = eval @@ ee_create r in
          (ed_of_value @@ Value.Cons (lvalue, rvalue), ECons, [ lderiv; rderiv ])
      | Expr.Match (e1, clauses) -> (
          let (value, _), deriv1 = eval @@ ee_create e1 in
          match mlver with
          | ML4 -> (
              match clauses with
              | [
               (Expr.NilPat, e2);
               (Expr.ConsPat (Expr.VarPat x, Expr.VarPat y), e3);
              ] -> (
                  match value with
                  | Value.Nil ->
                      let (value, _), deriv2 = eval @@ ee_create e2 in
                      (ed_of_value value, EMatchNil, [ deriv1; deriv2 ])
                  | Value.Cons (v1, v2) ->
                      let (value, _), deriv3 =
                        eval @@ ee_create ~env:((y, v2) :: (x, v1) :: env) e3
                      in
                      (ed_of_value value, EMatchCons, [ deriv1; deriv3 ])
                  | _ -> error @@ sprintf "%s is not list" (Expr.to_string e1) )
              | _ -> failwith "Illegal match clauses in ML4" )
          | ML5 -> (
              let ml5_match_root = Option.value ~default:expr ml5_match_root in
              match clauses with
              | (pat, e2) :: rest -> (
                  let m, deriv2 =
                    try ml5match pat value
                    with Failure s ->
                      raise
                      @@ Error
                           (sprintf "Pattern type error (%s)" s, ml5_match_root)
                  in
                  match m with
                  | Some env1 ->
                      let evaled, deriv3 =
                        eval @@ ee_create ~env:(env1 @ env) e2
                      in
                      ( evaled,
                        (if rest = [] then EMatchM1 else EMatchM2),
                        [ deriv1; deriv2; deriv3 ] )
                  | None ->
                      let evaled, deriv3 =
                        eval ~ml5_match_root
                        @@ ee_create (Expr.Match (e1, rest))
                      in
                      (evaled, EMatchN, [ deriv1; deriv2; deriv3 ]) )
              | _ -> raise @@ Error ("Match failure", ml5_match_root) )
          | _ -> failwith "Illegal match expression" )
    in
    (evaled, EDeriv.{ concl = EvalJ { evalee; evaled }; rule; premises })
  and ml5match p v =
    let matched, rule, premises =
      match (p, v) with
      | Expr.VarPat x, _ -> (Some [ (x, v) ], MVar, [])
      | Expr.NilPat, Value.Nil -> (Some [], MNil, [])
      | Expr.NilPat, Value.Cons _ -> (None, NMConsNil, [])
      | Expr.ConsPat (p1, p2), Value.Cons (v1, v2) -> (
          let m1, deriv1 = ml5match p1 v1 in
          let m2, deriv2 = ml5match p2 v2 in
          match (m1, m2) with
          | Some e1, Some e2 -> (Some (e2 @ e1), MCons, [ deriv1; deriv2 ])
          | None, _ -> (None, NMConsConsL, [ deriv1 ])
          | _, None -> (None, NMConsConsR, [ deriv2 ]) )
      | Expr.ConsPat _, Value.Nil -> (None, NMNilCons, [])
      | Expr.WildPat, _ -> (Some [], MWild, [])
      | _ -> failwith @@ sprintf "%s" (Expr.pat_to_string p)
    in
    (matched, EDeriv.{ concl = MatchJ (p, v, matched); rule; premises })
  in
  eval evalee
