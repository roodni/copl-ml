{
let reserved = [
  ("true", Parser.TRUE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("else", Parser.ELSE);
]
}

rule main = parse
  | [' ' '\t' '\n' '\r']+ { main lexbuf }
  | ['0'-'9']+ { Parser.INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | "<" { Parser.LT }
  | "+" { Parser.PLUS }
  | "-" { Parser.MINUS }
  | "*" { Parser.TIMES }
  | ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* {
      let id = Lexing.lexeme lexbuf in
      try List.assoc id reserved
      with Not_found -> Parser.ID id
    }
  | eof | ";;" { Parser.END }
  | "|-" { Parser.TURNSTILE }
  | "=" { Parser.EQ }
  | "," { Parser.COMMA }