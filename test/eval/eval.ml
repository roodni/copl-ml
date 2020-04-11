open OUnit2
open Evalml
open Evalml.Expr
open Evalml.Value

let eval_test system title value ?(stores = (Store.empty, Store.empty))
    ?(env = []) expr =
  let expected = Evaluated.to_string (value, fst stores) in
  let evaled, _ = Deriv.eval system { store = snd stores; env; expr } in
  title >:: fun _ ->
  assert_equal ~printer:(fun x -> x) expected (Evaluated.to_string evaled)

let plus (l, r) = BOpExp (PlusOp, l, r)

let minus (l, r) = BOpExp (MinusOp, l, r)

let times (l, r) = BOpExp (TimesOp, l, r)

let lt (l, r) = BOpExp (LtOp, l, r)

let assign (l, r) = BOpExp (AssignOp, l, r)

let var = Var.of_string

let varex s = VarExp (var s)

let varint (v, i) = (var v, IntVal i)

let call (s, e) = AppExp (varex s, e)

let lexp (v, e1, e2) = LetExp (var v, e1, e2)

let loc = Loc.of_string

let locv s = LocVal (loc s)

let locint (l, i) = (loc l, IntVal i)

let locloc (l1, l2) = (loc l1, locv l2)

let varloc (v, l) = (var v, locv l)

let refint i = RefExp (IntExp i)

let refvar v = RefExp (varex v)

let derefv s = DerefExp (varex s)

let cases_refml3 =
  [
    ( "Q141",
      IntVal 5,
      [ locint ("l", 2) ],
      [ locint ("l", 2) ],
      [ varloc ("x", "l") ],
      plus (derefv "x", IntExp 3) );
    ( "Q148",
      IntVal 3,
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
                          assign (derefv "refx", DerefExp (derefv "refy")),
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
           eval_test EvalRefML3 title value
             ~stores:(s_create s2, s_create s1)
             ~env expr)
         cases_refml3

let cases_ml3 =
  [
    ( "Q34",
      IntVal 3,
      [ (var "y", IntVal 2); (var "x", IntVal 3) ],
      VarExp (var "x") );
    ( "Q35",
      IntVal 5,
      [ (var "y", IntVal 4); (var "x", BoolVal true) ],
      IfExp
        ( VarExp (var "x"),
          BOpExp (PlusOp, VarExp (var "y"), IntExp 1),
          BOpExp (MinusOp, VarExp (var "y"), IntExp 1) ) );
    ( "Q36",
      IntVal 12,
      [],
      LetExp
        ( var "x",
          BOpExp (PlusOp, IntExp 1, IntExp 2),
          BOpExp (TimesOp, VarExp (var "x"), IntExp 4) ) );
    ( "Q39",
      IntVal 5,
      [],
      LetExp
        ( var "x",
          LetExp
            ( var "y",
              BOpExp (MinusOp, IntExp 3, IntExp 2),
              BOpExp (TimesOp, VarExp (var "y"), VarExp (var "y")) ),
          LetExp
            ( var "y",
              IntExp 4,
              BOpExp (PlusOp, VarExp (var "x"), VarExp (var "y")) ) ) );
    ( "Q40",
      FunVal ([], var "x", plus (varex "x", IntExp 1)),
      [],
      FunExp (var "x", plus (varex "x", IntExp 1)) );
    ( "Q41",
      FunVal ([ (var "y", IntVal 2) ], var "x", plus (varex "x", varex "y")),
      [],
      LetExp (var "y", IntExp 2, FunExp (var "x", plus (varex "x", varex "y")))
    );
    ( "Q42",
      IntVal 25,
      [],
      LetExp
        ( var "sq",
          FunExp (var "x", times (varex "x", varex "x")),
          plus (AppExp (varex "sq", IntExp 3), AppExp (varex "sq", IntExp 4)) )
    );
    ( "Q43",
      IntVal 25,
      [],
      LetExp
        ( var "sm",
          FunExp
            ( var "f",
              plus (AppExp (varex "f", IntExp 3), AppExp (varex "f", IntExp 4))
            ),
          AppExp (varex "sm", FunExp (var "x", times (varex "x", varex "x"))) )
    );
    ( "Q44",
      IntVal 5,
      [],
      LetExp
        ( var "max",
          FunExp
            ( var "x",
              FunExp
                ( var "y",
                  IfExp (lt (varex "x", varex "y"), varex "y", varex "x") ) ),
          AppExp (AppExp (varex "max", IntExp 3), IntExp 5) ) );
    ( "Q50",
      IntVal 6,
      [],
      LetRecExp
        ( var "fact",
          var "n",
          IfExp
            ( lt (varex "n", IntExp 2),
              IntExp 1,
              times (varex "n", call ("fact", minus (varex "n", IntExp 1))) ),
          call ("fact", IntExp 3) ) );
  ]

let tests_ml3 =
  "ML3"
  >::: List.map
         (fun (title, value, env, expr) ->
           eval_test EvalML3 title value ~env expr)
         cases_ml3

let cases_ml1 =
  [
    ("Q25", IntVal 8, BOpExp (PlusOp, IntExp 3, IntExp 5));
    ( "Q26",
      IntVal 3,
      BOpExp (MinusOp, BOpExp (MinusOp, IntExp 8, IntExp 2), IntExp 3) );
    ( "Q27",
      IntVal ~-81,
      BOpExp
        ( TimesOp,
          BOpExp (PlusOp, IntExp 4, IntExp 5),
          BOpExp (MinusOp, IntExp 1, IntExp 10) ) );
    ( "Q28",
      IntVal 5,
      IfExp
        ( BOpExp (LtOp, IntExp 4, IntExp 5),
          BOpExp (PlusOp, IntExp 2, IntExp 3),
          BOpExp (TimesOp, IntExp 8, IntExp 8) ) );
    ( "Q29",
      IntVal 11,
      BOpExp
        ( PlusOp,
          IntExp 3,
          IfExp
            ( BOpExp (LtOp, IntExp ~-23, BOpExp (TimesOp, IntExp ~-2, IntExp 8)),
              IntExp 8,
              BOpExp (PlusOp, IntExp 2, IntExp 4) ) ) );
    ( "Q30",
      IntVal 15,
      BOpExp
        ( PlusOp,
          BOpExp
            ( PlusOp,
              IntExp 3,
              IfExp
                ( BOpExp
                    (LtOp, IntExp ~-23, BOpExp (TimesOp, IntExp ~-2, IntExp 8)),
                  IntExp 8,
                  IntExp 2 ) ),
          IntExp 4 ) );
  ]

let tests_ml1 =
  "ML1"
  >::: List.map
         (fun (title, value, exp) -> eval_test EvalML1 title value exp)
         cases_ml1

let () = run_test_tt_main ("tests" >::: [ tests_ml1; tests_ml3; tests_refml3 ])
