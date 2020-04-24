open OUnit2
open Coplml

let parse_and_typing_test title str expected =
  title >:: fun _ ->
  let lexbuf = Lexing.from_string str in
  let toplevel = Parser.toplevel Lexer.main lexbuf in
  match toplevel with
  | Typing { tenv; expr } ->
      let _, ty, _ = Typing.typing tenv expr in
      assert_equal ~printer:Types.to_string expected ty
  | _ -> assert false

let cases =
  [
    ("Q80", "|- 3 + 5 : int", Types.Int);
    ("Q81", "|- if 4 < 5 then 2 + 3 else 8 * 8 : int", Types.Int);
    ("Q82", "x : bool, y : int |- if x then y + 1 else y - 1 : int", Types.Int);
    ("Q84", "|- fun x -> x + 1 : int -> int", Types.Fun (Int, Int));
    ("Q85", "|- let f = fun x -> x + 1 in f 4 : int", Types.Int);
    ( "Q86",
      "|- fun f -> f 0 + f 1 : (int -> int) -> int",
      Types.Fun (Fun (Int, Int), Int) );
    ( "Q87",
      "|- let max = fun x -> fun y -> if x < y then y else x in max 3 5 : int",
      Types.Int );
    ("Q88", "|- 4 :: [] : int list", Types.List Int);
    ( "Q89",
      "|- fun x -> fun y -> x : int -> int -> int",
      Types.Fun (Int, Fun (Int, Int)) );
    ( "Q90",
      "|- fun x -> fun y -> x : bool -> int -> bool",
      Types.Fun (Bool, Fun (Int, Bool)) );
    ("Q91", "|- let k = fun x -> fun y -> x in k 3 true : int", Types.Int);
    ( "Q99",
      "|- let rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1) in \
       fact 3 : int",
      Types.Int );
    ( "Q101",
      "|- let l = (fun x -> x) :: (fun y -> 2) :: (fun z -> z + 3) :: [] in 2 \
       : int",
      Types.Int );
    ( "Q102",
      "|- let rec length = fun l -> match l with [] -> 0 | x :: y -> 1 + \
       length y in length : int list -> int",
      Types.Fun (List Int, Int) );
  ]

let tests =
  "typing"
  >::: List.map
         (fun (title, str, ty) -> parse_and_typing_test title str ty)
         cases

let () = run_test_tt_main tests
