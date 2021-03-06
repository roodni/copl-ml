open OUnit2
open Base
open Evalml
open Table

let eval_test mlver title value ?(stores = (Store.empty, Store.empty))
    ?(env = []) expr =
  let evalee = Eval.ee_create mlver ~store:(snd stores) ~env expr in
  title
  >::: [
         ( "eval" >:: fun _ ->
           let expected = (value, fst stores) in
           let evaled, _ = Eval.eval evalee in
           assert_equal ~cmp:Eval.ed_equal ~printer:Eval.ed_to_string expected
             evaled );
         ( "reparse" >:: fun _ ->
           let toplevel =
             Cui.Parser.toplevel Cui.Lexer.main
               (Lexing.from_string @@ Eval.ee_to_string evalee)
           in
           match toplevel with
           | Eval { store; env; expr; _ } ->
               let evalee' = Eval.ee_create mlver ?store ?env expr in
               assert_equal ~printer:Eval.ee_to_string evalee evalee'
           | _ -> assert false );
       ]

let plus (l, r) = Expr.BOp (PlusOp, l, r)

let minus (l, r) = Expr.BOp (MinusOp, l, r)

let times (l, r) = Expr.BOp (TimesOp, l, r)

let lt (l, r) = Expr.BOp (LtOp, l, r)

let var = Var.of_string

let varex s = Expr.Var (var s)

let varint (v, i) = (var v, Value.Int i)

let lexp (v, e1, e2) = Expr.Let (var v, e1, e2)

let fexp (v, e) = Expr.Fun (var v, e)

let call (s, e) = Expr.App (varex s, e)

let loc = Loc.of_string

let locv s = Value.Loc (loc s)

let locint (l, i) = (loc l, Value.Int i)

let locloc (l1, l2) = (loc l1, locv l2)

let varloc (v, l) = (var v, locv l)

let refint i = Expr.Ref (Expr.Int i)

let refvar v = Expr.Ref (varex v)

let derefv s = Expr.Deref (varex s)

let assign (l, r) = Expr.Assign (l, r)

let cons (l, r) = Expr.Cons (l, r)

let icons (i, l) = cons (Expr.Int i, l)

let ml4match (e, nil_e, cons_v1, cons_v2, cons_e) =
  Expr.Match
    ( e,
      [
        (NilPat, nil_e);
        (ConsPat (VarPat (var cons_v1), VarPat (var cons_v2)), cons_e);
      ] )

let rec ilistv il =
  match il with
  | [] -> Value.Nil
  | i :: rest -> Value.Cons (Value.Int i, ilistv rest)

let cases_ml4 =
  [
    ( "Q70",
      ilistv [ 3; 7 ],
      [],
      cons
        ( plus (Expr.Int 1, Expr.Int 2),
          cons (plus (Expr.Int 3, Expr.Int 4), Expr.Nil) ) );
    ( "Q71",
      Value.Int 5,
      [],
      lexp
        ( "f",
          fexp ("x", ml4match (varex "x", Expr.Int 0, "a", "b", varex "a")),
          plus
            ( plus (call ("f", icons (4, Expr.Nil)), call ("f", Expr.Nil)),
              call ("f", icons (1, icons (2, icons (3, Expr.Nil)))) ) ) );
  ]

let tests_ml4 =
  "ML4"
  >::: List.map
         (fun (title, value, env, expr) -> eval_test ML4 title value ~env expr)
         cases_ml4

let cases_refml3 =
  [
    ( "Q141",
      Value.Int 5,
      [ locint ("l", 2) ],
      [ locint ("l", 2) ],
      [ varloc ("x", "l") ],
      plus (derefv "x", Expr.Int 3) );
    ( "Q148",
      Value.Int 3,
      [
        locint ("l1", 3);
        locint ("l2", 3);
        locloc ("l3", "l1");
        locloc ("l4", "l2");
      ],
      [],
      [],
      lexp
        ( "x",
          refint 2,
          lexp
            ( "y",
              refint 3,
              lexp
                ( "refx",
                  refvar "x",
                  lexp
                    ( "refy",
                      refvar "y",
                      lexp
                        ( "z",
                          assign (derefv "refx", Expr.Deref (derefv "refy")),
                          derefv "x" ) ) ) ) ) );
  ]

let tests_refml3 =
  "RefML3"
  >::: List.map
         (fun (title, value, s2, s1, env, expr) ->
           let s_create bs =
             let locs, values = List.split bs in
             Store.create locs values
           in
           eval_test RefML3 title value
             ~stores:(s_create s2, s_create s1)
             ~env expr)
         cases_refml3

let cases_ml3 =
  [
    ( "Q34",
      Value.Int 3,
      [ (var "y", Value.Int 2); (var "x", Value.Int 3) ],
      Expr.Var (var "x") );
    ( "Q35",
      Value.Int 5,
      [ (var "y", Value.Int 4); (var "x", Value.Bool true) ],
      Expr.If
        ( Expr.Var (var "x"),
          Expr.BOp (PlusOp, Expr.Var (var "y"), Expr.Int 1),
          Expr.BOp (MinusOp, Expr.Var (var "y"), Expr.Int 1) ) );
    ( "Q36",
      Value.Int 12,
      [],
      Expr.Let
        ( var "x",
          Expr.BOp (PlusOp, Expr.Int 1, Expr.Int 2),
          Expr.BOp (TimesOp, Expr.Var (var "x"), Expr.Int 4) ) );
    ( "Q39",
      Value.Int 5,
      [],
      Expr.Let
        ( var "x",
          Expr.Let
            ( var "y",
              Expr.BOp (MinusOp, Expr.Int 3, Expr.Int 2),
              Expr.BOp (TimesOp, Expr.Var (var "y"), Expr.Var (var "y")) ),
          Expr.Let
            ( var "y",
              Expr.Int 4,
              Expr.BOp (PlusOp, Expr.Var (var "x"), Expr.Var (var "y")) ) ) );
    ( "Q40",
      Value.Fun ([], var "x", plus (varex "x", Expr.Int 1)),
      [],
      Expr.Fun (var "x", plus (varex "x", Expr.Int 1)) );
    ( "Q41",
      Value.Fun
        ([ (var "y", Value.Int 2) ], var "x", plus (varex "x", varex "y")),
      [],
      Expr.Let
        (var "y", Expr.Int 2, Expr.Fun (var "x", plus (varex "x", varex "y")))
    );
    ( "Q42",
      Value.Int 25,
      [],
      Expr.Let
        ( var "sq",
          Expr.Fun (var "x", times (varex "x", varex "x")),
          plus
            ( Expr.App (varex "sq", Expr.Int 3),
              Expr.App (varex "sq", Expr.Int 4) ) ) );
    ( "Q43",
      Value.Int 25,
      [],
      Expr.Let
        ( var "sm",
          Expr.Fun
            ( var "f",
              plus
                ( Expr.App (varex "f", Expr.Int 3),
                  Expr.App (varex "f", Expr.Int 4) ) ),
          Expr.App (varex "sm", Expr.Fun (var "x", times (varex "x", varex "x")))
        ) );
    ( "Q44",
      Value.Int 5,
      [],
      Expr.Let
        ( var "max",
          Expr.Fun
            ( var "x",
              Expr.Fun
                ( var "y",
                  Expr.If (lt (varex "x", varex "y"), varex "y", varex "x") ) ),
          Expr.App (Expr.App (varex "max", Expr.Int 3), Expr.Int 5) ) );
    ( "Q50",
      Value.Int 6,
      [],
      Expr.LetRec
        ( var "fact",
          var "n",
          Expr.If
            ( lt (varex "n", Expr.Int 2),
              Expr.Int 1,
              times (varex "n", call ("fact", minus (varex "n", Expr.Int 1))) ),
          call ("fact", Expr.Int 3) ) );
  ]

let tests_ml3 =
  "ML3"
  >::: List.map
         (fun (title, value, env, expr) -> eval_test ML3 title value ~env expr)
         cases_ml3

let cases_ml1 =
  [
    ("Q25", Value.Int 8, Expr.BOp (PlusOp, Expr.Int 3, Expr.Int 5));
    ( "Q26",
      Value.Int 3,
      Expr.BOp (MinusOp, Expr.BOp (MinusOp, Expr.Int 8, Expr.Int 2), Expr.Int 3)
    );
    ( "Q27",
      Value.Int ~-81,
      Expr.BOp
        ( TimesOp,
          Expr.BOp (PlusOp, Expr.Int 4, Expr.Int 5),
          Expr.BOp (MinusOp, Expr.Int 1, Expr.Int 10) ) );
    ( "Q28",
      Value.Int 5,
      Expr.If
        ( Expr.BOp (LtOp, Expr.Int 4, Expr.Int 5),
          Expr.BOp (PlusOp, Expr.Int 2, Expr.Int 3),
          Expr.BOp (TimesOp, Expr.Int 8, Expr.Int 8) ) );
    ( "Q29",
      Value.Int 11,
      Expr.BOp
        ( PlusOp,
          Expr.Int 3,
          Expr.If
            ( Expr.BOp
                ( LtOp,
                  Expr.Int ~-23,
                  Expr.BOp (TimesOp, Expr.Int ~-2, Expr.Int 8) ),
              Expr.Int 8,
              Expr.BOp (PlusOp, Expr.Int 2, Expr.Int 4) ) ) );
    ( "Q30",
      Value.Int 15,
      Expr.BOp
        ( PlusOp,
          Expr.BOp
            ( PlusOp,
              Expr.Int 3,
              Expr.If
                ( Expr.BOp
                    ( LtOp,
                      Expr.Int ~-23,
                      Expr.BOp (TimesOp, Expr.Int ~-2, Expr.Int 8) ),
                  Expr.Int 8,
                  Expr.Int 2 ) ),
          Expr.Int 4 ) );
  ]

let tests_ml1 =
  "ML1"
  >::: List.map
         (fun (title, value, exp) -> eval_test ML1 title value exp)
         cases_ml1

let () =
  run_test_tt_main
    ("tests" >::: [ tests_ml1; tests_ml3; tests_refml3; tests_ml4 ])
