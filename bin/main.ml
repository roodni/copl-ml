open Coplml
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
  match toplevel with
  | Eval { store; env; expr; is_judg } ->
      let mlver =
        try Mlver.detect ?store ?env expr with
        | Mlver.Error (v1, v2) ->
            eprintf "ML version detection failed: %s | %s\n"
              (Mlver.to_string v1) (Mlver.to_string v2);
            exit 1
        | Mlver.Empty_match_clauses e ->
            eprintf "Empty match clauses: %s\n" (Expr.to_string e);
            exit 1
      in
      eprintf "ML version: %s\n" (Mlver.to_string mlver);
      flush stderr;
      let evalee = Eval.ee_create mlver ?store ?env expr in
      let evaled, deriv =
        try Eval.eval evalee
        with Eval.Error (er, ex) ->
          eprintf "%s: %s\n" er (Expr.to_string ex);
          exit 1
      in
      let deriv =
        if mlver = RefML3 && is_judg then
          let rec get_locs num =
            if num = 0 then []
            else
              let loc =
                try Parser.loc_name Lexer.main lexbuf with
                | Failure e ->
                    eprintf "%s\n" e;
                    exit 1
                | Parser.Error ->
                    eprintf "Syntax error\n";
                    exit 1
              in
              loc :: get_locs (num - 1)
          in
          let locnum = evaled |> snd |> Store.binds |> List.length in
          let locs = get_locs locnum in
          let values =
            Option.value ~default:Store.empty store
            |> Store.binds |> List.split |> snd
          in
          Eval.ee_create mlver ~store:(Store.create locs values) ?env expr
          |> Eval.eval |> snd
        else deriv
      in
      Eval.EDeriv.output deriv
  | Typing { tenv; expr } ->
      let _, _, deriv =
        try Typing.typing tenv expr with
        | Typing.Typing_failed ->
            eprintf "Typing failed\n";
            exit 1
        | Typing.Expr_error (s, e) ->
            eprintf "%s: %s\n" s (Expr.to_string e);
            exit 1
      in
      Typing.TDeriv.output deriv
