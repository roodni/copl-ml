open OUnit2
open Evalml.Expr
open Evalml.Evaluatee
open Evalml.Var
open Evalml.Value
open Evalml.Env

(* 環境を含む *)
let cases_env =
  [
    ( "Q34",
      [ (Var "y", IntVal 2); (Var "x", IntVal 3) ],
      VarExp (Var "x"),
      "x = 3, y = 2 |- x" );
    ( "Q35",
      [ (Var "y", IntVal 4); (Var "x", BoolVal true) ],
      IfExp
        ( VarExp (Var "x"),
          BOpExp (PlusOp, VarExp (Var "y"), IntExp 1),
          BOpExp (MinusOp, VarExp (Var "y"), IntExp 1) ),
      "x = true, y = 4 |- if x then y + 1 else y - 1" );
    ( "Q36",
      [],
      LetExp
        ( Var "x",
          BOpExp (PlusOp, IntExp 1, IntExp 2),
          BOpExp (TimesOp, VarExp (Var "x"), IntExp 4) ),
      "|- let x = 1 + 2 in x * 4" );
    ( "Q39",
      [],
      LetExp
        ( Var "x",
          LetExp
            ( Var "y",
              BOpExp (MinusOp, IntExp 3, IntExp 2),
              BOpExp (TimesOp, VarExp (Var "y"), VarExp (Var "y")) ),
          LetExp
            ( Var "y",
              IntExp 4,
              BOpExp (PlusOp, VarExp (Var "x"), VarExp (Var "y")) ) ),
      "|- let x = let y = 3 - 2 in y * y in let y = 4 in x + y" );
  ]

(* パースが正しいか調べる *)
let parse_env_test env expr str _ =
  let { env = env'; expr = expr' } =
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
  let s = evaluatee_to_string evalee in
  let evalee' =
    Evalml.Parser.toplevel Evalml.Lexer.main (Lexing.from_string s)
  in
  assert_equal ~printer:evaluatee_to_string evalee evalee'

let evalee_to_string_tests =
  "evaluatee_to_string"
  >::: List.map (fun (title, evalee) -> title >:: evalee_to_string_test evalee)
       @@ List.map
            (fun (title, env, expr, _) -> (title, { env; expr }))
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
    ("infix_plus prefix_minus", BOpExp (PlusOp, IntExp 1, IntExp ~-2), "1 + - 2");
    ( "infix_minus prefix_minus",
      BOpExp (MinusOp, IntExp 1, IntExp ~-2),
      "1 - - 2" );
    ( "plus times plus",
      BOpExp
        ( PlusOp,
          BOpExp (PlusOp, IntExp 1, BOpExp (TimesOp, IntExp 2, IntExp ~-3)),
          IntExp 4 ),
      "1 + 2 * -3 + 4" );
  ]

(* パースが正しいか調べる *)
let parse_expr_test expr str _ =
  let { env = _; expr = parsed } =
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
