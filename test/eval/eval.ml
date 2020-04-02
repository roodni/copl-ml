open OUnit2
open Evalml.Expr
open Evalml.Value
open Evalml.Evaluatee
open Evalml.Deriv
open Evalml.Var

let eval_test value ?(env = []) expr _ =
  let evaled, _ = eval_to_deriv { env; expr } in
  assert_equal ~printer:value_to_string value evaled

let cases_env =
  [
    ( "Q34",
      IntVal 3,
      [ (Var "y", IntVal 2); (Var "x", IntVal 3) ],
      VarExp (Var "x") );
    ( "Q35",
      IntVal 5,
      [ (Var "y", IntVal 4); (Var "x", BoolVal true) ],
      IfExp
        ( VarExp (Var "x"),
          BOpExp (PlusOp, VarExp (Var "y"), IntExp 1),
          BOpExp (MinusOp, VarExp (Var "y"), IntExp 1) ) );
  ]

let eval_env_tests =
  "eval env and expr"
  >::: List.map
         (fun (title, value, env, expr) -> title >:: eval_test value ~env expr)
         cases_env

(* exprだけ *)
let cases_expr =
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

let eval_expr_tests =
  "eval EvalML1"
  >::: List.map
         (fun (title, value, exp) -> title >:: eval_test value exp)
         cases_expr

let () = run_test_tt_main ("tests" >::: [ eval_expr_tests; eval_env_tests ])
