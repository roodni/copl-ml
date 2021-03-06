open Base
open Printf

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
    | TAbs
    | TMult

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
    | TAbs -> "T-Abs"
    | TMult -> "T-Mult"

  type judgment = Tenv.t * Expr.t * Types.t

  let judgment_to_string (tenv, expr, ty) =
    let s_env = if tenv = [] then "" else Tenv.to_string tenv ^ " " in
    sprintf "%s|- %s : %s" s_env (Expr.to_string expr) (Types.to_string ty)
end

module TDeriv = Deriv.Make (System)

let rec substitute_deriv sub TDeriv.{ concl = tenv, expr, ty; rule; premises } =
  let tenv = Tenv.substitute sub tenv
  and ty = Tsub.substitute ty sub
  and premises = List.map (substitute_deriv sub) premises in
  TDeriv.{ concl = (tenv, expr, ty); rule; premises }

exception Typing_failed

exception Expr_error of string * Expr.t

let typing ~poly tenv expr =
  let open System in
  let clojure ty tenv =
    let ftv = Tvset.diff (Types.ftv ty) (Tenv.ftv tenv) in
    Tscheme.create (Tvset.elements ftv) ty
  in
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
          (sub, tt, TIf, [ cderiv; tderiv; fderiv ])
      | Expr.BOp (op, e1, e2) ->
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
            | TimesOp -> if poly then TMult else TTimes
            | LtOp -> TLt
          in
          (sub, ty, rule, [ deriv1; deriv2 ])
      | Expr.Var v ->
          let scheme =
            try List.assoc v tenv
            with Not_found -> raise @@ Expr_error ("Undeclared variable", expr)
          in
          let ty = Tscheme.generate_instance scheme in
          (Tsub.empty, ty, TVar, [])
      | Expr.Let (v, e1, e2) ->
          let s1, t1, deriv1 = principal tenv e1 in
          let scheme =
            if poly then clojure t1 (Tenv.substitute s1 tenv)
            else Tscheme.simple t1
          in
          let s2, t2, deriv2 = principal ((v, scheme) :: tenv) e2 in
          let sub =
            [ s1; s2 ] |> List.map Teqs.of_tsub |> Teqs.union_list |> Teqs.unify
          in
          (sub, t2, TLet, [ deriv1; deriv2 ])
      | Expr.Fun (v, e) ->
          let a = Types.Var (Tvar.generate ()) in
          let sub, ty, deriv = principal ((v, Tscheme.simple a) :: tenv) e in
          (sub, Types.Fun (a, ty), (if poly then TAbs else TFun), [ deriv ])
      | Expr.App (e1, e2) ->
          let s1, t1, deriv1 = principal tenv e1
          and s2, t2, deriv2 = principal tenv e2 in
          let a = Types.Var (Tvar.generate ()) in
          let sub =
            [ s1; s2 ] |> List.map Teqs.of_tsub |> Teqs.union_list
            |> Teqs.add (t1, Types.Fun (t2, a))
            |> Teqs.unify
          in
          (sub, a, TApp, [ deriv1; deriv2 ])
      | Expr.LetRec (f, v, e1, e2) ->
          let a = Types.Var (Tvar.generate ())
          and b = Types.Var (Tvar.generate ()) in
          let s1, t1, deriv1 =
            principal
              ((v, Tscheme.simple b) :: (f, Tscheme.simple a) :: tenv)
              e1
          in
          let s2 =
            s1 |> Teqs.of_tsub |> Teqs.add (a, Types.Fun (b, t1)) |> Teqs.unify
          in
          let scheme =
            if poly then
              clojure (Tsub.substitute a s2) (Tenv.substitute s2 tenv)
            else Tscheme.simple a
          in
          let s3, t3, deriv3 = principal ((f, scheme) :: tenv) e2 in
          let s4 =
            [ s2; s3 ] |> List.map Teqs.of_tsub |> Teqs.union_list |> Teqs.unify
          in
          (s4, t3, TLetRec, [ deriv1; deriv3 ])
      | Expr.Nil ->
          let a = Types.Var (Tvar.generate ()) in
          (Tsub.empty, Types.List a, TNil, [])
      | Expr.Cons (e1, e2) ->
          let s1, t1, deriv1 = principal tenv e1
          and s2, t2, deriv2 = principal tenv e2 in
          let sub =
            [ s1; s2 ] |> List.map Teqs.of_tsub |> Teqs.union_list
            |> Teqs.add (t2, Types.List t1)
            |> Teqs.unify
          in
          (sub, t2, TCons, [ deriv1; deriv2 ])
      | Expr.Match (e1, [ (NilPat, e2); (ConsPat (VarPat va, VarPat vb), e3) ])
        ->
          let a = Types.Var (Tvar.generate ()) in
          let s1, t1, deriv1 = principal tenv e1
          and s2, t2, deriv2 = principal tenv e2
          and s3, t3, deriv3 =
            principal
              ( (vb, Tscheme.simple @@ Types.List a)
              :: (va, Tscheme.simple a)
              :: tenv )
              e3
          in
          let sub =
            [ s1; s2; s3 ] |> List.map Teqs.of_tsub |> Teqs.union_list
            |> Teqs.add (t1, Types.List a)
            |> Teqs.add (t2, t3)
            |> Teqs.unify
          in
          (sub, t2, TMatch, [ deriv1; deriv2; deriv3 ])
      | _ -> raise @@ Expr_error ("Not ML4 expression", expr)
    in
    ( sub,
      Tsub.substitute ty sub,
      TDeriv.{ concl = (tenv, expr, ty); rule; premises } )
  in
  let sub, ty, deriv =
    try principal tenv expr with Teqs.Unify_failed -> raise Typing_failed
  in
  let deriv = substitute_deriv sub deriv in
  let deriv =
    if poly then deriv
    else
      (* 型変数をすべてboolで埋める *)
      let interface_ftv = Types.ftv ty in
      let rec substitute_inside
          TDeriv.{ concl = tenv, expr, ty; rule; premises } =
        let rec substitute = function
          | (Types.Int | Types.Bool) as x -> x
          | Types.Fun (x, y) -> Types.Fun (substitute x, substitute y)
          | Types.List x -> Types.List (substitute x)
          | Types.Var v ->
              if Tvset.mem v interface_ftv then Types.Var v else Types.Bool
        in
        let tenv =
          List.map
            (fun (v, scheme) ->
              ( v,
                scheme |> Tscheme.generate_instance |> substitute
                |> Tscheme.simple ))
            tenv
        and ty = substitute ty
        and premises = List.map substitute_inside premises in
        TDeriv.{ concl = (tenv, expr, ty); rule; premises }
      in
      substitute_inside deriv
  in
  (sub, ty, deriv)
