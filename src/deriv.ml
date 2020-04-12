open Expr
open Value
open Printf

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

type judgment =
  | EvalJ of { evalee : Evalee.t; evaled : Evaled.t }
  | PlusJ of int * int * int
  | MinusJ of int * int * int
  | TimesJ of int * int * int
  | LtJ of int * int * bool

let judgment_to_string = function
  | EvalJ { evalee; evaled } ->
      sprintf "%s evalto %s" (Evalee.to_string evalee) (Evaled.to_string evaled)
  | PlusJ (l, r, s) -> sprintf "%d plus %d is %d" l r s
  | MinusJ (l, r, d) -> sprintf "%d minus %d is %d" l r d
  | TimesJ (l, r, p) -> sprintf "%d times %d is %d" l r p
  | LtJ (l, r, b) -> sprintf "%d less than %d is %b" l r b

type t = { concl : judgment; rule : rule; premises : t list }

let rec output ?(indent = 0) ?(outchan = stdout) { premises; rule; concl } =
  let printf f = fprintf outchan f in
  let rec output_indent depth =
    if depth > 0 then (
      printf "  ";
      output_indent (depth - 1) )
  in
  output_indent indent;
  printf "%s by %s " (judgment_to_string concl) (rule_to_string rule);
  if premises = [] then printf "{};\n"
  else (
    printf "{\n";
    List.iter (fun deriv -> output ~indent:(indent + 1) ~outchan deriv) premises;
    output_indent indent;
    printf "};\n" )

exception EvalError of string * Expr.expr

let eval system evalee =
  let rec eval evalee =
    let Evalee.{ store; env; expr } = evalee in
    let error s = raise @@ EvalError (s, expr) in
    let evaled, rule, premises =
      match expr with
      | IntExp i -> ((IntVal i, store), EInt, [])
      | BoolExp b -> ((BoolVal b, store), EBool, [])
      | IfExp (c, t, f) ->
          let (cvalue, store), cderiv = eval { evalee with expr = c } in
          let retexpr, rule =
            match cvalue with
            | BoolVal true -> (t, EIfT)
            | BoolVal false -> (f, EIfF)
            | _ -> error "Condition must be bool"
          in
          let ret, retderiv = eval { evalee with store; expr = retexpr } in
          (ret, rule, [ cderiv; retderiv ])
      | BOpExp (((PlusOp | MinusOp | TimesOp | LtOp) as op), lexpr, rexpr) -> (
          let (lvalue, store), lderiv = eval { evalee with expr = lexpr } in
          let (rvalue, store), rderiv =
            eval { evalee with store; expr = rexpr }
          in
          match (lvalue, rvalue) with
          | IntVal li, IntVal ri ->
              let value, erule, bjudg, brule =
                match op with
                | PlusOp ->
                    let i = li + ri in
                    (IntVal i, EPlus, PlusJ (li, ri, i), BPlus)
                | MinusOp ->
                    let i = li - ri in
                    (IntVal i, EMinus, MinusJ (li, ri, i), BMinus)
                | TimesOp ->
                    let i = li * ri in
                    let erule, brule =
                      match system with
                      | System.EvalRefML3 -> (EMult, BMult)
                      | _ -> (ETimes, BTimes)
                    in
                    (IntVal i, erule, TimesJ (li, ri, i), brule)
                | LtOp ->
                    let b = li < ri in
                    (BoolVal b, ELt, LtJ (li, ri, b), BLt)
                | _ -> assert false
              in
              ( (value, store),
                erule,
                [
                  lderiv; rderiv; { concl = bjudg; rule = brule; premises = [] };
                ] )
          | _ -> error "Both arguments must be int" )
      | BOpExp (AssignOp, lexpr, rexpr) -> (
          let (lvalue, store), lderiv = eval { evalee with expr = lexpr } in
          let (rvalue, store), rderiv =
            eval { evalee with store; expr = rexpr }
          in
          match lvalue with
          | LocVal loc ->
              let store =
                try Store.assign store loc rvalue
                with Store.Invalid_reference -> error "Invalid reference"
              in
              ((rvalue, store), EAssign, [ lderiv; rderiv ])
          | _ -> error (sprintf "%s is not loc" (expr_to_string lexpr)) )
      | VarExp v -> (
          match system with
          | System.EvalRefML3 ->
              (* 1 step var *)
              let value =
                try List.assoc v env
                with Not_found -> error "Undeclared variable"
              in
              ((value, store), EVar, [])
          | _ -> (
              match env with
              | (v', value) :: _ when v = v' ->
                  (Evaled.of_value value, EVar1, [])
              | (_, _) :: tail ->
                  let evaled, premise = eval { evalee with env = tail } in
                  (evaled, EVar2, [ premise ])
              | [] -> error "Undeclared variable" ) )
      | LetExp (v, e1, e2) ->
          let (value1, store), deriv1 = eval { evalee with expr = e1 } in
          let (value2, store), deriv2 =
            eval { store; env = (v, value1) :: env; expr = e2 }
          in
          ((value2, store), ELet, [ deriv1; deriv2 ])
      | FunExp (v, e) -> ((FunVal (env, v, e), store), EFun, [])
      | AppExp (e1, e2) -> (
          let (fval, store), fderiv = eval { evalee with expr = e1 } in
          let (aval, store), aderiv = eval { evalee with store; expr = e2 } in
          match fval with
          | FunVal (fenv, avar, fexpr) ->
              let evaled, deriv =
                eval { store; env = (avar, aval) :: fenv; expr = fexpr }
              in
              (evaled, EApp, [ fderiv; aderiv; deriv ])
          | RecFunVal (fenv, fvar, avar, fexpr) ->
              let evaled, deriv =
                eval
                  {
                    store;
                    env = (avar, aval) :: (fvar, fval) :: fenv;
                    expr = fexpr;
                  }
              in
              (evaled, EAppRec, [ fderiv; aderiv; deriv ])
          | _ -> error (sprintf "%s cannot be applied" (expr_to_string e1)) )
      | LetRecExp (f, a, e1, e2) ->
          let evaled, premise =
            eval
              {
                evalee with
                env = (f, RecFunVal (env, f, a, e1)) :: env;
                expr = e2;
              }
          in
          (evaled, ELetRec, [ premise ])
      | RefExp e ->
          let (value, store), premise = eval { evalee with expr = e } in
          let loc, store = Store.make_ref store value in
          ((LocVal loc, store), ERef, [ premise ])
      | DerefExp e -> (
          let (value, store), premise = eval { evalee with expr = e } in
          match value with
          | LocVal loc ->
              let value =
                try Store.deref store loc
                with Store.Invalid_reference -> error "Invalid reference"
              in
              ((value, store), EDeref, [ premise ])
          | _ -> error (sprintf "%s must be loc" (expr_to_string e)) )
    in
    (evaled, { concl = EvalJ { evalee; evaled }; rule; premises })
  in
  eval evalee
