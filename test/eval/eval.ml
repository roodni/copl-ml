open OUnit2
open Evalml
open Evalml.Expr
open Evalml.Value

let eval_test value ?(env = []) expr _ =
  let evaled, _ = Deriv.eval { env; expr } in
  assert_equal ~printer:value_to_string value evaled

let var = Var.of_string

let cases_env =
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
