open OUnit2
open Evalml.Exp
open Evalml.Value
open Evalml.Evaluatee
open Evalml.Deriv

let cases =
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

let eval_exp_test value exp _ =
  let evaled, _ = eval_to_deriv { env = []; exp } in
  assert_equal ~printer:value_to_string value evaled

let eval_exp_tests =
  "eval EvalML1"
  >::: List.map
         (fun (title, value, exp) -> title >:: eval_exp_test value exp)
         cases

let () = run_test_tt_main eval_exp_tests
