open OUnit2
open Evalml.Expr
open Evalml.Evaluatee

let cases =
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

let parse_expr_test expr str _ =
  let { env = _; expr = parsed } =
    Evalml.Parser.toplevel Evalml.Lexer.main (Lexing.from_string str)
  in
  assert_equal ~printer:expr_to_string expr parsed

let parse_expr_tests =
  "parse"
  >::: List.map
         (fun (title, exp, str) -> title >:: parse_expr_test exp str)
         cases

(* expを文字列化して再度パースし、同じexpになるかどうか調べる *)
let expr_to_string_test exp =
  let s = expr_to_string exp in
  parse_expr_test exp s

let expr_to_string_tests =
  "exp_to_string"
  >::: List.map (fun (title, exp, _) -> title >:: expr_to_string_test exp) cases

let () =
  run_test_tt_main ("tests" >::: [ parse_expr_tests; expr_to_string_tests ])
