open OUnit2
open Evalml.Syntax

let parse_test str exp _ =
  let parsed =
    Evalml.Parser.toplevel Evalml.Lexer.main (Lexing.from_string str)
  in
  assert_equal ~printer:exp_to_s_string exp parsed

let parse_tests =
  "parse tests"
  >::: [
         "Q25" >:: parse_test "3 + 5" (BOpExp (PlusOp, IntExp 3, IntExp 5));
         "Q26"
         >:: parse_test "8 - 2 - 3"
               (BOpExp (MinusOp, BOpExp (MinusOp, IntExp 8, IntExp 2), IntExp 3));
         "Q27"
         >:: parse_test "(4 + 5) * (1 - 10)"
               (BOpExp
                  ( TimesOp,
                    BOpExp (PlusOp, IntExp 4, IntExp 5),
                    BOpExp (MinusOp, IntExp 1, IntExp 10) ));
         "Q28"
         >:: parse_test "if 4 < 5 then 2 + 3 else 8 * 8"
               (IfExp
                  ( BOpExp (LtOp, IntExp 4, IntExp 5),
                    BOpExp (PlusOp, IntExp 2, IntExp 3),
                    BOpExp (TimesOp, IntExp 8, IntExp 8) ));
         "Q29"
         >:: parse_test "3 + if -23 < -2 * 8 then 8 else 2 + 4"
               (BOpExp
                  ( PlusOp,
                    IntExp 3,
                    IfExp
                      ( BOpExp
                          ( LtOp,
                            IntExp ~-23,
                            BOpExp (TimesOp, IntExp ~-2, IntExp 8) ),
                        IntExp 8,
                        BOpExp (PlusOp, IntExp 2, IntExp 4) ) ));
         "Q30"
         >:: parse_test "3 + (if -23 < -2 * 8 then 8 else 2) + 4"
               (BOpExp
                  ( PlusOp,
                    BOpExp
                      ( PlusOp,
                        IntExp 3,
                        IfExp
                          ( BOpExp
                              ( LtOp,
                                IntExp ~-23,
                                BOpExp (TimesOp, IntExp ~-2, IntExp 8) ),
                            IntExp 8,
                            IntExp 2 ) ),
                    IntExp 4 ));
         "Q31"
         >:: parse_test "1 + true + 2"
               (BOpExp
                  (PlusOp, BOpExp (PlusOp, IntExp 1, BoolExp true), IntExp 2));
         "Q32"
         >:: parse_test "if 2 + 3 then 1 else 3"
               (IfExp (BOpExp (PlusOp, IntExp 2, IntExp 3), IntExp 1, IntExp 3));
         "Q33"
         >:: parse_test "if 3 < 4 then 1 < true else 3 - false"
               (IfExp
                  ( BOpExp (LtOp, IntExp 3, IntExp 4),
                    BOpExp (LtOp, IntExp 1, BoolExp true),
                    BOpExp (MinusOp, IntExp 3, BoolExp false) ));
         "infix_plus prefix_minus"
         >:: parse_test "1 + - 2" (BOpExp (PlusOp, IntExp 1, IntExp ~-2));
         "infix_minus prefix_minus"
         >:: parse_test "1 - - 2" (BOpExp (MinusOp, IntExp 1, IntExp ~-2));
         "plus times plus"
         >:: parse_test "1 + 2 * -3 + 4"
               (BOpExp
                  ( PlusOp,
                    BOpExp
                      (PlusOp, IntExp 1, BOpExp (TimesOp, IntExp 2, IntExp ~-3)),
                    IntExp 4 ));
       ]

let () = run_test_tt_main parse_tests
