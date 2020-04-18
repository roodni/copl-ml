open Evalml
open Printf

let () =
  let lexbuf = Lexing.from_channel stdin in
  let toplevel =
    try Parser.toplevel Lexer.main lexbuf with
    | Failure e ->
        eprintf "%s\n" e;
        exit 1
    | Parser.Error ->
        eprintf "Syntax error\n";
        exit 1
  in
  let mlver =
    try Mlver.detect toplevel with
    | Mlver.Error (v1, v2) ->
        eprintf "ML version detection failed: %s | %s\n" (Mlver.to_string v1)
          (Mlver.to_string v2);
        exit 1
    | Mlver.Empty_match_clauses e ->
        eprintf "Empty match clauses: %s\n" (Expr.to_string e);
        exit 1
  in
  eprintf "ML version: %s\n" (Mlver.to_string mlver);
  flush stderr;
  let evalee =
    Eval.ee_create mlver ?store:toplevel.store ?env:toplevel.env toplevel.expr
  in
  let evaled, deriv =
    try Eval.eval evalee
    with Eval.Error (er, ex) ->
      eprintf "%s: %s\n" er (Expr.to_string ex);
      exit 1
  in
  let deriv =
    if mlver = RefML3 && toplevel.input_type = Judg then
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
      let locnum = evaled |> snd |> Store.binds |> List.length in
      let locs = get_locs locnum in
      let values =
        Option.value ~default:Store.empty toplevel.store
        |> Store.binds |> List.split |> snd
      in
      Eval.ee_create mlver ~store:(Store.create locs values) ?env:toplevel.env
        toplevel.expr
      |> Eval.eval |> snd
    else deriv
  in
  Eval.EDeriv.output deriv
