open Printf

module Ftv = struct
  include Set.Make (Tvar)

  let rec of_types = function
    | Types.Int | Types.Bool -> empty
    | Types.Fun (x, y) -> union (of_types x) (of_types y)
    | Types.List x -> of_types x
    | Types.Var v -> singleton v
end

module Teqs = struct
  include Set.Make (struct
    type t = Types.t * Types.t

    let compare = compare
  end)

  let of_tsub sub =
    Tsub.fold (fun v ty eqs -> add (Types.Var v, ty) eqs) sub empty

  let union_list l = List.fold_left (fun t1 t2 -> union t1 t2) empty l

  exception Unify_failed

  let rec unify eqs =
    match choose_opt eqs with
    | None -> Tsub.empty
    | Some (ty1, ty2) -> (
        let eqs = remove (ty1, ty2) eqs in
        match (ty1, ty2) with
        | x, y when x = y -> unify eqs
        | Types.Var v, ty | ty, Types.Var v ->
            if Ftv.mem v (Ftv.of_types ty) then raise Unify_failed
            else
              let sub = Tsub.singleton v ty in
              let s =
                unify
                  (map
                     (fun (ty1, ty2) ->
                       (Tsub.substitute ty1 sub, Tsub.substitute ty2 sub))
                     eqs)
              in
              Tsub.composite s sub
        | Types.Fun (ty11, ty12), Types.Fun (ty21, ty22) ->
            unify (union eqs (of_list [ (ty11, ty21); (ty12, ty22) ]))
        | Types.List ty1, Types.List ty2 -> unify (add (ty1, ty2) eqs)
        | _ -> raise Unify_failed )
end

module System = struct
  type rule =
    | TInt
    | TBool
    | TIf
    | TPlus
    | TMinus
    | TTimes
    | TLt
    | TVar
    | TLet
    | TFun
    | TApp
    | TLetRec
    | TNil
    | TCons
    | TMatch

  let rule_to_string = function
    | TInt -> "T-Int"
    | TBool -> "T-Bool"
    | TIf -> "T-If"
    | TPlus -> "T-Plus"
    | TMinus -> "T-Minus"
    | TTimes -> "T-Times"
    | TLt -> "T-Lt"
    | TVar -> "T-Var"
    | TLet -> "T-Let"
    | TFun -> "T-Fun"
    | TApp -> "T-App"
    | TLetRec -> "T-LetRec"
    | TNil -> "T-Nil"
    | TCons -> "T-Cons"
    | TMatch -> "T-Match"

  type judgment = Tenv.t * Expr.t * Types.t

  let judgment_to_string (tenv, expr, ty) =
    let s_env = if tenv = [] then "" else Tenv.to_string tenv ^ " " in
    sprintf "%s|- %s : %s" s_env (Expr.to_string expr) (Types.to_string ty)
end

module TDeriv = Deriv.Make (System)

exception Typing_failed

exception Expr_error of string * Expr.t

let typing tenv expr =
  let open System in
  let rec principal tenv expr =
    let sub, ty, rule, premises =
      match expr with
      | Expr.Int _ -> (Tsub.empty, Types.Int, TInt, [])
      | Expr.Bool _ -> (Tsub.empty, Types.Bool, TBool, [])
      | Expr.If (c, t, f) ->
          let cs, ct, cderiv = principal tenv c
          and ts, tt, tderiv = principal tenv t
          and fs, ft, fderiv = principal tenv f in
          let sub =
            [ cs; ts; fs ] |> List.map Teqs.of_tsub |> Teqs.union_list
            |> Teqs.add (ct, Types.Bool)
            |> Teqs.add (tt, ft)
            |> Teqs.unify
          in
          (sub, Tsub.substitute tt sub, TIf, [ cderiv; tderiv; fderiv ])
      | Expr.BOp (((PlusOp | MinusOp | TimesOp | LtOp) as op), e1, e2) ->
          let s1, t1, deriv1 = principal tenv e1
          and s2, t2, deriv2 = principal tenv e2 in
          let sub =
            [ s1; s2 ] |> List.map Teqs.of_tsub |> Teqs.union_list
            |> Teqs.add (t1, Types.Int)
            |> Teqs.add (t2, Types.Int)
            |> Teqs.unify
          and ty = if op = LtOp then Types.Bool else Types.Int
          and rule =
            match op with
            | PlusOp -> TPlus
            | MinusOp -> TMinus
            | TimesOp -> TTimes
            | LtOp -> TLt
            | _ -> assert false
          in
          (sub, Tsub.substitute ty sub, rule, [ deriv1; deriv2 ])
      | Expr.Var v ->
          let ty =
            try List.assoc v tenv
            with Not_found -> raise @@ Expr_error ("Undeclared variable", expr)
          in
          (Tsub.empty, ty, TVar, [])
      | Expr.Let (v, e1, e2) ->
          let s1, t1, deriv1 = principal tenv e1 in
          let s2, t2, deriv2 = principal ((v, t1) :: tenv) e2 in
          let sub =
            [ s1; s2 ] |> List.map Teqs.of_tsub |> Teqs.union_list |> Teqs.unify
          in
          (sub, Tsub.substitute t2 sub, TLet, [ deriv1; deriv2 ])
      | Expr.Fun (v, e) ->
          let a = Types.Var (Tvar.create ()) in
          let sub, ty, deriv = principal ((v, a) :: tenv) e in
          (sub, Types.Fun (Tsub.substitute a sub, ty), TFun, [ deriv ])
      | Expr.App (e1, e2) ->
          let s1, t1, deriv1 = principal tenv e1
          and s2, t2, deriv2 = principal tenv e2 in
          let a = Types.Var (Tvar.create ()) in
          let sub =
            [ s1; s2 ] |> List.map Teqs.of_tsub |> Teqs.union_list
            |> Teqs.add (t1, Types.Fun (t2, a))
            |> Teqs.unify
          in
          (sub, Tsub.substitute a sub, TApp, [ deriv1; deriv2 ])
      | Expr.LetRec (v, f, e1, e2) ->
          let a = Types.Var (Tvar.create ())
          and b = Types.Var (Tvar.create ()) in
          let s1, t1, deriv1 = principal ((v, b) :: (f, a) :: tenv) e1 in
          let s2, t2, deriv2 = principal ((f, a) :: tenv) e2 in
          let sub =
            [ s1; s2 ] |> List.map Teqs.of_tsub |> Teqs.union_list
            |> Teqs.add (a, Types.Fun (b, t1))
            |> Teqs.unify
          in
          (sub, Tsub.substitute t2 sub, TLetRec, [ deriv1; deriv2 ])
      | Expr.Nil ->
          let a = Types.Var (Tvar.create ()) in
          (Tsub.empty, Types.List a, TNil, [])
      | Expr.BOp (ConsOp, e1, e2) ->
          let s1, t1, deriv1 = principal tenv e1
          and s2, t2, deriv2 = principal tenv e2 in
          let sub =
            [ s1; s2 ] |> List.map Teqs.of_tsub |> Teqs.union_list
            |> Teqs.add (t2, Types.List t1)
            |> Teqs.unify
          in
          (sub, Tsub.substitute t2 sub, TCons, [ deriv1; deriv2 ])
      | Expr.Match (e1, [ (NilPat, e2); (ConsPat (VarPat va, VarPat vb), e3) ])
        ->
          let a = Types.Var (Tvar.create ()) in
          let s1, t1, deriv1 = principal tenv e1
          and s2, t2, deriv2 = principal tenv e2
          and s3, t3, deriv3 =
            principal ((vb, Types.List a) :: (va, a) :: tenv) e3
          in
          let sub =
            [ s1; s2; s3 ] |> List.map Teqs.of_tsub |> Teqs.union_list
            |> Teqs.add (t1, Types.List a)
            |> Teqs.add (t2, t3)
            |> Teqs.unify
          in
          (sub, Tsub.substitute t2 sub, TMatch, [ deriv1; deriv2; deriv3 ])
      | _ -> raise @@ Expr_error ("Not ML4 expression", expr)
    in
    (sub, ty, TDeriv.{ concl = (tenv, expr, ty); rule; premises })
  in
  let sub, _, deriv =
    try principal tenv expr with Teqs.Unify_failed -> raise Typing_failed
  in
  let rec substitute_deriv TDeriv.{ concl = tenv, expr, ty; rule; premises } =
    let tenv = Tsub.substitute_env tenv sub
    and ty = Tsub.substitute ty sub
    and premises = List.map substitute_deriv premises in
    TDeriv.{ concl = (tenv, expr, ty); rule; premises }
  in
  substitute_deriv deriv
