open Deriv

let eval_input_to_deriv () =
  let exp =
    try Parser.toplevel Lexer.main (Lexing.from_channel stdin) with
    | Failure e ->
        Printf.eprintf "%s\n" e;
        exit 1
    | Parser.Error | Not_found ->
        Printf.eprintf "parsing: error\n";
        exit 1
  in
  let _, deriv =
    try eval_to_deriv exp
    with EvalError e ->
      Printf.eprintf "%s\n" e;
      exit 1
  in
  output_deriv deriv
