let rec read_print_loop () =
  print_string "# ";
  flush stdout;
  ( try
      let e = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
      print_endline (Syntax.exp_to_s_string e)
    with
  | Failure e -> print_endline e
  | Parser.Error -> print_endline "parsing: error"
  | Not_found -> print_endline "not found" );
  read_print_loop ()
