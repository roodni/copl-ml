{
let reserved = [
  ("evalto", Parser.END);
  ("true", Parser.TRUE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("else", Parser.ELSE);
  ("let", Parser.LET);
  ("in", Parser.IN);
  ("fun", Parser.FUN);
  ("rec", Parser.REC)
]

exception NoSuchKeyword of string
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
      with Not_found -> raise (NoSuchKeyword id)
    }
  | eof | ";;" { Parser.END }
  | "|-" { Parser.TURNSTILE }
  | "=" { Parser.EQ }
  | "," { Parser.COMMA }
  | "->" { Parser.RIGHTARROW }
  | "[" { Parser.LBRACKET }
  | "]" { Parser.RBRACKET }
  | "." { Parser.DOT }
  | "#" ['1'-'9'] ['0'-'9']* {
      let s = Lexing.lexeme lexbuf in
      let n = int_of_string (String.sub s 1 (String.length s - 1)) in
      Parser.ID n
    }