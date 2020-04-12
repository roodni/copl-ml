open Printf

let eval_input_to_deriv () =
  let lexbuf = Lexing.from_channel stdin in
  let toplevel =
    try Parser.toplevel Lexer.main lexbuf with
    | Failure e ->
        eprintf "%s\n" e;
        exit 1
    | Parser.Error ->
        eprintf "syntax error\n";
        exit 1
  in
  let system = System.EvalML4 in
  let evalee = Toplevel.to_evalee toplevel in
  let evaled, deriv =
    try Deriv.eval system evalee
    with Deriv.EvalError (er, ex) ->
      eprintf "%s: %s\n" er (Expr.to_string ex);
      exit 1
  in
  let deriv =
    if system = EvalRefML3 && Toplevel.is_judg toplevel then
      let rec get_locs num =
        if num = 0 then []
        else
          let loc =
            try Parser.loc_name Lexer.main lexbuf with
            | Failure e ->
                eprintf "%s\n" e;
                exit 1
            | Parser.Error ->
                eprintf "syntax error\n";
                exit 1
          in
          loc :: get_locs (num - 1)
      in
      let locnum = Evaled.store evaled |> Store.binds |> List.length in
      let locs = get_locs locnum in
      let values = Store.binds evalee.store |> List.split |> snd in
      Deriv.eval system { evalee with store = Store.create locs values } |> snd
    else deriv
  in
  Deriv.output deriv
