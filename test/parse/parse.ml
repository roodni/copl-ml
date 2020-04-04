open OUnit2
open Evalml
open Evalml.Expr
open Evalml.Value

let var = Var.of_string

let varex s = VarExp (Var.of_string s)

let plus (l, r) = BOpExp (PlusOp, l, r)

let minus (l, r) = BOpExp (MinusOp, l, r)

let times (l, r) = BOpExp (TimesOp, l, r)

let lt (l, r) = BOpExp (LtOp, l, r)

(* 環境を含む *)
let cases_env =
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
  ]

(* パースが正しいか調べる *)
let parse_env_test env expr str _ =
  let Evaluatee.{ env = env'; expr = expr' } =
    Evalml.Parser.toplevel Evalml.Lexer.main (Lexing.from_string str)
  in
  assert_equal
    ~printer:(fun (env, expr) ->
      Printf.sprintf "%s |- %s" (env_to_string env) (expr_to_string expr))
    (env, expr) (env', expr')

let parse_env_tests =
  "parse env and expr"
  >::: List.map
         (fun (title, env, expr, str) -> title >:: parse_env_test env expr str)
         cases_env

(* evaleeを文字列化して再度パースし、一致するか調べる *)
let evalee_to_string_test evalee _ =
  let s = Evaluatee.to_string evalee in
  let evalee' =
    Evalml.Parser.toplevel Evalml.Lexer.main (Lexing.from_string s)
  in
  assert_equal ~printer:Evaluatee.to_string evalee evalee'

let evalee_to_string_tests =
  "evaluatee_to_string"
  >::: List.map (fun (title, evalee) -> title >:: evalee_to_string_test evalee)
       @@ List.map
            (fun (title, env, expr, _) -> (title, Evaluatee.{ env; expr }))
            cases_env

(* exprだけ (おもにEvalML1) *)
let cases_expr =
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

(* パースが正しいか調べる *)
let parse_expr_test expr str _ =
  let Evaluatee.{ env = _; expr = parsed } =
    Evalml.Parser.toplevel Evalml.Lexer.main (Lexing.from_string str)
  in
  assert_equal ~printer:expr_to_string expr parsed

let parse_expr_tests =
  "parse expr"
  >::: List.map
         (fun (title, exp, str) -> title >:: parse_expr_test exp str)
         cases_expr

(* exprを文字列化して再度パースし、同じexpになるかどうか調べる *)
let expr_to_string_test exp =
  let s = expr_to_string exp in
  parse_expr_test exp s

let expr_to_string_tests =
  "expr_to_string"
  >::: List.map
         (fun (title, exp, _) -> title >:: expr_to_string_test exp)
         cases_expr

let () =
  run_test_tt_main
    ( "tests"
    >::: [
           parse_expr_tests;
           expr_to_string_tests;
           parse_env_tests;
           evalee_to_string_tests;
         ] )
