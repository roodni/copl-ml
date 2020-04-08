open OUnit2
open Evalml
open Value

let tests =
  [
    ( "empty" >:: fun _ ->
      let s = Store.create [] [] in
      assert_bool "empty" (Store.is_empty s);
      assert_equal (Store.to_string s) "" );
    ( "single" >:: fun _ ->
      let s = Store.create [] [ IntVal 1 ] in
      assert_equal (Store.to_string s) "@l1 = 1" );
    ( "triple" >:: fun _ ->
      let s = Store.create [] [ IntVal 1; IntVal 2; IntVal 3 ] in
      assert_equal (Store.to_string s) "@l1 = 1, @l2 = 2, @l3 = 3" );
    ( "assign" >:: fun _ ->
      let s = Store.create [] [ IntVal 1; IntVal 2; IntVal 3 ] in
      let s = Store.assign s (Loc.of_string "l2") (IntVal 100) in
      assert_equal (Store.to_string s) "@l1 = 1, @l2 = 100, @l3 = 3" );
    ( "locpool" >:: fun _ ->
      let s =
        Store.create
          (List.map Loc.of_string [ "x"; "l1"; "y"; "l3" ])
          (List.map (fun i -> IntVal i) [ 1; 2; 3; 4; 5 ])
      in
      assert_equal (Store.to_string s)
        "@x = 1, @l1 = 2, @y = 3, @l3 = 4, @l2 = 5" );
  ]

let () = run_test_tt_main ("store" >::: tests)
