let eval_input_to_deriv () =
  let toplevel =
    try Parser.toplevel Lexer.main (Lexing.from_channel stdin) with
    | Failure e ->
        Printf.eprintf "%s\n" e;
        exit 1
    | Parser.Error ->
        Printf.eprintf "parsing: error\n";
        exit 1
  in
  let evalee = Toplevel.to_evaluatee toplevel in
  let _, deriv =
    try Deriv.eval evalee
    with Deriv.EvalError e ->
      Printf.eprintf "%s\n" e;
      exit 1
  in
  Deriv.output deriv
