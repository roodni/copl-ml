open OUnit2
open Evalml
open Evalml.Expr
open Evalml.Value

let parse_and_reparse_test title ?(store = Store.empty) ?(env = []) expr str =
  let expected = Evaluatee.{ store; env; expr } in
  let parse s =
    Toplevel.to_evaluatee @@ Parser.toplevel Lexer.main (Lexing.from_string s)
  in
  title
  >::: [
         ( "parse" >:: fun _ ->
           assert_equal ~printer:Evaluatee.to_string expected (parse str) );
         ( "reparse" >:: fun _ ->
           assert_equal ~printer:Evaluatee.to_string expected
             (Evaluatee.to_string expected |> parse) );
       ]

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
      [ locint ("l", 2) ],
      [ varloc ("x", "l") ],
      plus (derefv "x", IntExp 3),
      "@l = 2 / x = @l |- !x + 3" );
    ( "Q148",
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
                          derefv "x" ) ) ) ) ),
      "|- let x = ref 2 in let y = ref 3 in let refx = ref x in let refy = ref \
       y in let z = !refx := !(!refy) in !x" );
    ( "tri",
      [ locint ("l1", 1); locint ("l2", 2); locint ("l3", 3) ],
      [],
      IntExp 4,
      "@l1 = 1, @l2 = 2, @l3 = 3 / |- 4" );
  ]

let tests_refml3 =
  "RefML3"
  >::: List.map
         (fun (title, store, env, exp, str) ->
           let store =
             let locs, values = List.split store in
             Store.create locs values
           in
           parse_and_reparse_test title ~store ~env exp str)
         cases_refml3

(* 環境を含む *)
let cases_ml3 =
  [
    ( "Q34",
      [ (var "y", IntVal 2); (var "x", IntVal 3) ],
      VarExp (var "x"),
      "x = 3, y = 2 |- x" );
    ( "Q35",
      [ (var "y", IntVal 4); (var "x", BoolVal true) ],
      IfExp
        ( VarExp (var "x"),
          BOpExp (PlusOp, VarExp (var "y"), IntExp 1),
          BOpExp (MinusOp, VarExp (var "y"), IntExp 1) ),
      "x = true, y = 4 |- if x then y + 1 else y - 1" );
    ( "Q36",
      [],
      LetExp
        ( var "x",
          BOpExp (PlusOp, IntExp 1, IntExp 2),
          BOpExp (TimesOp, VarExp (var "x"), IntExp 4) ),
      "|- let x = 1 + 2 in x * 4" );
    ( "Q39",
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
              BOpExp (PlusOp, VarExp (var "x"), VarExp (var "y")) ) ),
      "|- let x = let y = 3 - 2 in y * y in let y = 4 in x + y" );
    ( "Q40",
      [],
      FunExp (var "x", plus (varex "x", IntExp 1)),
      "|- fun x -> x + 1" );
    ( "Q41",
      [],
      LetExp (var "y", IntExp 2, FunExp (var "x", plus (varex "x", varex "y"))),
      "|- let y = 2 in fun x -> x + y" );
    ( "Q42",
      [],
      LetExp
        ( var "sq",
          FunExp (var "x", times (varex "x", varex "x")),
          plus (AppExp (varex "sq", IntExp 3), AppExp (varex "sq", IntExp 4)) ),
      "|- let sq = fun x -> x * x in sq 3 + sq 4" );
    ( "Q43",
      [],
      LetExp
        ( var "sm",
          FunExp
            ( var "f",
              plus (AppExp (varex "f", IntExp 3), AppExp (varex "f", IntExp 4))
            ),
          AppExp (varex "sm", FunExp (var "x", times (varex "x", varex "x"))) ),
      "|- let sm = fun f -> f 3 + f 4 in sm (fun x -> x * x)" );
    ( "Q44",
      [],
      LetExp
        ( var "max",
          FunExp
            ( var "x",
              FunExp
                ( var "y",
                  IfExp (lt (varex "x", varex "y"), varex "y", varex "x") ) ),
          AppExp (AppExp (varex "max", IntExp 3), IntExp 5) ),
      "|- let max = fun x -> fun y -> if x < y then y else x in max 3 5" );
    ("1 + f 2", [], plus (IntExp 1, AppExp (varex "f", IntExp 2)), "1 + f 2");
    ( "fun env",
      [
        ( var "f",
          FunVal
            ( [ (var "y", IntVal 2); (var "x", IntVal 1) ],
              var "z",
              plus (plus (varex "x", varex "y"), varex "z") ) );
      ],
      AppExp (varex "f", IntExp 3),
      "f = (x = 1, y = 2)[fun z -> x + y + z] |- f 3" );
    ( "Q50",
      [],
      LetRecExp
        ( var "fact",
          var "n",
          IfExp
            ( lt (varex "n", IntExp 2),
              IntExp 1,
              times (varex "n", call ("fact", minus (varex "n", IntExp 1))) ),
          call ("fact", IntExp 3) ),
      "|- let rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1) in \
       fact 3" );
    ( "rec fun env",
      [
        ( var "fact",
          RecFunVal
            ( [],
              var "fact",
              var "n",
              IfExp
                ( lt (varex "n", IntExp 2),
                  IntExp 1,
                  times (varex "n", call ("fact", minus (varex "n", IntExp 1)))
                ) ) );
      ],
      call ("fact", IntExp 3),
      "fact=()[rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1)] |- \
       fact 3" );
  ]

let tests_ml3 =
  "ML3"
  >::: List.map
         (fun (title, env, expr, str) ->
           parse_and_reparse_test title ~env expr str)
         cases_ml3

let cases_ml1 =
  [
    ("Q25", BOpExp (PlusOp, IntExp 3, IntExp 5), "3 + 5");
    ( "Q26",
      BOpExp (MinusOp, BOpExp (MinusOp, IntExp 8, IntExp 2), IntExp 3),
      "8 - 2 - 3" );
    ( "Q27",
      BOpExp
        ( TimesOp,
          BOpExp (PlusOp, IntExp 4, IntExp 5),
          BOpExp (MinusOp, IntExp 1, IntExp 10) ),
      "(4 + 5) * (1 - 10)" );
    ( "Q28",
      IfExp
        ( BOpExp (LtOp, IntExp 4, IntExp 5),
          BOpExp (PlusOp, IntExp 2, IntExp 3),
          BOpExp (TimesOp, IntExp 8, IntExp 8) ),
      "if 4 < 5 then 2 + 3 else 8 * 8" );
    ( "Q29",
      BOpExp
        ( PlusOp,
          IntExp 3,
          IfExp
            ( BOpExp (LtOp, IntExp ~-23, BOpExp (TimesOp, IntExp ~-2, IntExp 8)),
              IntExp 8,
              BOpExp (PlusOp, IntExp 2, IntExp 4) ) ),
      "3 + if -23 < -2 * 8 then 8 else 2 + 4" );
    ( "Q30",
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
          IntExp 4 ),
      "3 + (if -23 < -2 * 8 then 8 else 2) + 4" );
    ( "Q31",
      BOpExp (PlusOp, BOpExp (PlusOp, IntExp 1, BoolExp true), IntExp 2),
      "1 + true + 2" );
    ( "Q32",
      IfExp (BOpExp (PlusOp, IntExp 2, IntExp 3), IntExp 1, IntExp 3),
      "if 2 + 3 then 1 else 3" );
    ( "Q33",
      IfExp
        ( BOpExp (LtOp, IntExp 3, IntExp 4),
          BOpExp (LtOp, IntExp 1, BoolExp true),
          BOpExp (MinusOp, IntExp 3, BoolExp false) ),
      "if 3 < 4 then 1 < true else 3 - false" );
    ("infix_plus prefix_minus", BOpExp (PlusOp, IntExp 1, IntExp ~-2), "1+-2");
    ("infix_minus prefix_minus", BOpExp (MinusOp, IntExp 1, IntExp ~-2), "1--2");
    ( "plus times plus",
      BOpExp
        ( PlusOp,
          BOpExp (PlusOp, IntExp 1, BOpExp (TimesOp, IntExp 2, IntExp ~-3)),
          IntExp 4 ),
      "1 + 2 * -3 + 4" );
  ]

let tests_ml1 =
  "ML1"
  >::: List.map
         (fun (title, exp, str) -> parse_and_reparse_test title exp str)
         cases_ml1

let () = run_test_tt_main ("tests" >::: [ tests_ml1; tests_ml3; tests_refml3 ])
