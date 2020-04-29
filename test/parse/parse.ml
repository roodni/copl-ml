open OUnit2
open Base
open Evalml

let parse_test title ?store ?env expr str =
  let expected = Cui.Toplevel.create_eval ?store ?env expr ~is_judg:false in
  let parse s = Cui.Parser.toplevel Cui.Lexer.main (Lexing.from_string s) in
  title >:: fun _ -> assert_equal expected (parse str)

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
      [],
      cons
        ( plus (Expr.Int 1, Expr.Int 2),
          cons (plus (Expr.Int 3, Expr.Int 4), Expr.Nil) ),
      "|- (1 + 2) :: (3 + 4) :: []" );
    ( "Q71",
      [],
      lexp
        ( "f",
          fexp ("x", ml4match (varex "x", Expr.Int 0, "a", "b", varex "a")),
          plus
            ( plus (call ("f", icons (4, Expr.Nil)), call ("f", Expr.Nil)),
              call ("f", icons (1, icons (2, icons (3, Expr.Nil)))) ) ),
      "|- let f = fun x -> match x with [] -> 0 | a :: b -> a in f (4::[]) + f \
       [] + f (1 :: 2 :: 3 :: [])" );
    ( "nested match",
      [],
      ml4match
        ( Expr.Nil,
          ml4match (Expr.Nil, Expr.Int 1, "x", "y", varex "x"),
          "a",
          "b",
          Expr.If
            ( Expr.Bool false,
              varex "a",
              ml4match (Expr.Nil, Expr.Int 2, "c", "d", varex "c") ) ),
      "|- match [] with [] -> (match [] with [] -> 1 | x :: y -> x) | a :: b \
       -> if false then a else (match [] with [] -> 2 | c :: d -> c)" );
    ("env", [ (var "x", ilistv [ 2; 1 ]) ], varex "x", "x = 2 :: 1 :: [] |- x");
    ( "nested list",
      [
        ( var "x",
          Value.Cons
            ( Value.Int 1,
              Value.Cons (Value.Cons (Value.Int 2, Value.Int 3), Value.Nil) ) );
      ],
      varex "x",
      "x = 1 :: (2 :: 3) :: [] |- x" );
  ]

let tests_ml4 =
  "ML4"
  >::: List.map
         (fun (title, env, expr, str) -> parse_test title ~env expr str)
         cases_ml4

let cases_refml3 =
  [
    ( "Q141",
      [ locint ("l", 2) ],
      [ varloc ("x", "l") ],
      plus (derefv "x", Expr.Int 3),
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
                          assign (derefv "refx", Expr.Deref (derefv "refy")),
                          derefv "x" ) ) ) ) ),
      "|- let x = ref 2 in let y = ref 3 in let refx = ref x in let refy = ref \
       y in let z = !refx := !(!refy) in !x" );
    ( "tri",
      [ locint ("l1", 1); locint ("l2", 2); locint ("l3", 3) ],
      [],
      Expr.Int 4,
      "@l1 = 1, @l2 = 2, @l3 = 3 / |- 4" );
  ]

let tests_refml3 =
  "RefML3"
  >::: List.map
         (fun (title, store, env, exp, str) ->
           let store =
             if store = [] then None
             else
               let locs, values = List.split store in
               Some (Store.create locs values)
           in
           parse_test title ?store ~env exp str)
         cases_refml3

(* 環境を含む *)
let cases_ml3 =
  [
    ( "Q34",
      [ (var "y", Value.Int 2); (var "x", Value.Int 3) ],
      Expr.Var (var "x"),
      "x = 3, y = 2 |- x" );
    ( "Q35",
      [ (var "y", Value.Int 4); (var "x", Value.Bool true) ],
      Expr.If
        ( Expr.Var (var "x"),
          Expr.BOp (PlusOp, Expr.Var (var "y"), Expr.Int 1),
          Expr.BOp (MinusOp, Expr.Var (var "y"), Expr.Int 1) ),
      "x = true, y = 4 |- if x then y + 1 else y - 1" );
    ( "Q36",
      [],
      Expr.Let
        ( var "x",
          Expr.BOp (PlusOp, Expr.Int 1, Expr.Int 2),
          Expr.BOp (TimesOp, Expr.Var (var "x"), Expr.Int 4) ),
      "|- let x = 1 + 2 in x * 4" );
    ( "Q39",
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
              Expr.BOp (PlusOp, Expr.Var (var "x"), Expr.Var (var "y")) ) ),
      "|- let x = let y = 3 - 2 in y * y in let y = 4 in x + y" );
    ( "Q40",
      [],
      Expr.Fun (var "x", plus (varex "x", Expr.Int 1)),
      "|- fun x -> x + 1" );
    ( "Q41",
      [],
      Expr.Let
        (var "y", Expr.Int 2, Expr.Fun (var "x", plus (varex "x", varex "y"))),
      "|- let y = 2 in fun x -> x + y" );
    ( "Q42",
      [],
      Expr.Let
        ( var "sq",
          Expr.Fun (var "x", times (varex "x", varex "x")),
          plus
            ( Expr.App (varex "sq", Expr.Int 3),
              Expr.App (varex "sq", Expr.Int 4) ) ),
      "|- let sq = fun x -> x * x in sq 3 + sq 4" );
    ( "Q43",
      [],
      Expr.Let
        ( var "sm",
          Expr.Fun
            ( var "f",
              plus
                ( Expr.App (varex "f", Expr.Int 3),
                  Expr.App (varex "f", Expr.Int 4) ) ),
          Expr.App (varex "sm", Expr.Fun (var "x", times (varex "x", varex "x")))
        ),
      "|- let sm = fun f -> f 3 + f 4 in sm (fun x -> x * x)" );
    ( "Q44",
      [],
      Expr.Let
        ( var "max",
          Expr.Fun
            ( var "x",
              Expr.Fun
                ( var "y",
                  Expr.If (lt (varex "x", varex "y"), varex "y", varex "x") ) ),
          Expr.App (Expr.App (varex "max", Expr.Int 3), Expr.Int 5) ),
      "|- let max = fun x -> fun y -> if x < y then y else x in max 3 5" );
    ( "1 + f 2",
      [],
      plus (Expr.Int 1, Expr.App (varex "f", Expr.Int 2)),
      "|- 1 + f 2" );
    ( "fun env",
      [
        ( var "f",
          Value.Fun
            ( [ (var "y", Value.Int 2); (var "x", Value.Int 1) ],
              var "z",
              plus (plus (varex "x", varex "y"), varex "z") ) );
      ],
      Expr.App (varex "f", Expr.Int 3),
      "f = (x = 1, y = 2)[fun z -> x + y + z] |- f 3" );
    ( "Q50",
      [],
      Expr.LetRec
        ( var "fact",
          var "n",
          Expr.If
            ( lt (varex "n", Expr.Int 2),
              Expr.Int 1,
              times (varex "n", call ("fact", minus (varex "n", Expr.Int 1))) ),
          call ("fact", Expr.Int 3) ),
      "|- let rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1) in \
       fact 3" );
    ( "rec fun env",
      [
        ( var "fact",
          Value.RecFun
            ( [],
              var "fact",
              var "n",
              Expr.If
                ( lt (varex "n", Expr.Int 2),
                  Expr.Int 1,
                  times (varex "n", call ("fact", minus (varex "n", Expr.Int 1)))
                ) ) );
      ],
      call ("fact", Expr.Int 3),
      "fact=()[rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1)] |- \
       fact 3" );
  ]

let tests_ml3 =
  "ML3"
  >::: List.map
         (fun (title, env, expr, str) -> parse_test title ~env expr str)
         cases_ml3

let cases_ml1 =
  [
    ("Q25", Expr.BOp (PlusOp, Expr.Int 3, Expr.Int 5), "3 + 5");
    ( "Q26",
      Expr.BOp (MinusOp, Expr.BOp (MinusOp, Expr.Int 8, Expr.Int 2), Expr.Int 3),
      "8 - 2 - 3" );
    ( "Q27",
      Expr.BOp
        ( TimesOp,
          Expr.BOp (PlusOp, Expr.Int 4, Expr.Int 5),
          Expr.BOp (MinusOp, Expr.Int 1, Expr.Int 10) ),
      "(4 + 5) * (1 - 10)" );
    ( "Q28",
      Expr.If
        ( Expr.BOp (LtOp, Expr.Int 4, Expr.Int 5),
          Expr.BOp (PlusOp, Expr.Int 2, Expr.Int 3),
          Expr.BOp (TimesOp, Expr.Int 8, Expr.Int 8) ),
      "if 4 < 5 then 2 + 3 else 8 * 8" );
    ( "Q29",
      Expr.BOp
        ( PlusOp,
          Expr.Int 3,
          Expr.If
            ( Expr.BOp
                ( LtOp,
                  Expr.Int ~-23,
                  Expr.BOp (TimesOp, Expr.Int ~-2, Expr.Int 8) ),
              Expr.Int 8,
              Expr.BOp (PlusOp, Expr.Int 2, Expr.Int 4) ) ),
      "3 + if -23 < -2 * 8 then 8 else 2 + 4" );
    ( "Q30",
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
          Expr.Int 4 ),
      "3 + (if -23 < -2 * 8 then 8 else 2) + 4" );
    ( "Q31",
      Expr.BOp
        (PlusOp, Expr.BOp (PlusOp, Expr.Int 1, Expr.Bool true), Expr.Int 2),
      "1 + true + 2" );
    ( "Q32",
      Expr.If (Expr.BOp (PlusOp, Expr.Int 2, Expr.Int 3), Expr.Int 1, Expr.Int 3),
      "if 2 + 3 then 1 else 3" );
    ( "Q33",
      Expr.If
        ( Expr.BOp (LtOp, Expr.Int 3, Expr.Int 4),
          Expr.BOp (LtOp, Expr.Int 1, Expr.Bool true),
          Expr.BOp (MinusOp, Expr.Int 3, Expr.Bool false) ),
      "if 3 < 4 then 1 < true else 3 - false" );
    ( "infix_plus prefix_minus",
      Expr.BOp (PlusOp, Expr.Int 1, Expr.Int ~-2),
      "1+-2" );
    ( "infix_minus prefix_minus",
      Expr.BOp (MinusOp, Expr.Int 1, Expr.Int ~-2),
      "1--2" );
    ( "plus times plus",
      Expr.BOp
        ( PlusOp,
          Expr.BOp
            (PlusOp, Expr.Int 1, Expr.BOp (TimesOp, Expr.Int 2, Expr.Int ~-3)),
          Expr.Int 4 ),
      "1 + 2 * -3 + 4" );
  ]

let tests_ml1 =
  "ML1"
  >::: List.map (fun (title, exp, str) -> parse_test title exp str) cases_ml1

let () =
  run_test_tt_main
    ("tests" >::: [ tests_ml1; tests_ml3; tests_refml3; tests_ml4 ])
