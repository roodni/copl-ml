{
let reserved = [
  ("true", Parser.TRUE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("else", Parser.ELSE);
  ("let", Parser.LET);
  ("in", Parser.IN);
  ("fun", Parser.FUN);
  ("rec", Parser.REC);
  ("ref", Parser.REF);
  ("evalto", Parser.EVALTO);
  ("match", Parser.MATCH);
  ("with", Parser.WITH);
  ("int", Parser.INTT);
  ("bool", Parser.BOOLT);
  ("list", Parser.LISTT);
  ("_", Parser.UNDER);
]
}

rule main = parse
  | [' ' '\t' '\n' '\r']+ { main lexbuf }
  | ['-']? ['0'-'9']+ { Parser.INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | "<" { Parser.LT }
  | "+" { Parser.PLUS }
  | "-" { Parser.MINUS }
  | "*" { Parser.TIMES }
  | ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* {
      let id = Lexing.lexeme lexbuf in
      try List.assoc id reserved
      with Not_found -> Parser.VAR id
    }
  | eof | ";;" { Parser.END }
  | "|-" { Parser.TURNSTILE }
  | "=" { Parser.EQ }
  | "," { Parser.COMMA }
  | "->" { Parser.RIGHTARROW }
  | "[" { Parser.LBRACKET }
  | "]" { Parser.RBRACKET }
  | "@" ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* {
      let s = Lexing.lexeme lexbuf in
      let id = String.sub s 1 (String.length s - 1) in
      Parser.LOC id
    }
  | ":=" { Parser.ASSIGN }
  | "!" { Parser.DEREF }
  | "/" { Parser.SLASH }
  | "::" { Parser.CONS }
  | "[]" { Parser.NIL }
  | "|" { Parser.BAR }
  | ":" { Parser.COLON }
  | "'" ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* {
      let s = Lexing.lexeme lexbuf in
      let id = String.sub s 1 (String.length s - 1) in
      Parser.TVAR id
    }
  | "." { Parser.DOT }
  | ">>" { Parser.GTGT }
  | "{" { Parser.LBRACE }
  | "}" { Parser.RBRACE }