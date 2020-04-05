%{
open Expr
open Value
%}

%token END
%token <int> INT
%token TRUE FALSE
%token LPAREN RPAREN
%token LT PLUS MINUS TIMES
%token IF THEN ELSE
%token TURNSTILE
%token <int> ID
%token EQ COMMA
%token LET IN
%token FUN RIGHTARROW
%token LBRACKET RBRACKET
%token REC
%token DOT

%nonassoc prec_let prec_fun prec_letrec
%nonassoc prec_if
%left LT
%left PLUS MINUS
%left TIMES
%nonassoc INT TRUE FALSE LPAREN ID

%start toplevel
%type <Evaluatee.t> toplevel
%%

toplevel :
  | en=env TURNSTILE ex=expr END { {env = en; expr = ex} }
  /* | ex=expr END { {env = []; expr = ex} } */

env :
  | { [] }
  | v=value { [v] }
  | e=env COMMA v=value { v :: e }

value :
  | i=INT { IntVal i }
  | TRUE { BoolVal true }
  | FALSE { BoolVal false }
  | LPAREN en=env RPAREN LBRACKET FUN DOT RIGHTARROW ex=expr RBRACKET
      { FunVal (en, ex) }
  | LPAREN en=env RPAREN LBRACKET REC DOT EQ FUN DOT RIGHTARROW ex=expr RBRACKET
      { RecFunVal (en, ex) }

expr :
  | IF c=expr THEN t=expr ELSE f=expr %prec prec_if { IfExp (c, t, f) }
  | l=expr LT r=expr { BOpExp (LtOp, l, r) }
  | l=expr PLUS r=expr { BOpExp (PlusOp, l, r) }
  | l=expr MINUS r=expr { BOpExp (MinusOp, l, r) }
  | l=expr TIMES r=expr { BOpExp (TimesOp, l, r) }
  | LET DOT EQ e1=expr IN e2=expr %prec prec_let { LetExp (e1, e2) }
  | FUN DOT RIGHTARROW e=expr %prec prec_fun { FunExp e }
  | l=expr r=simple { AppExp (l, r) }
  | e=simple { e }
  | LET REC DOT EQ FUN DOT RIGHTARROW e1=expr IN e2=expr %prec prec_letrec
      { LetRecExp (e1, e2) }

simple :
  | i=INT { IntExp i }
  | TRUE { BoolExp true }
  | FALSE { BoolExp false }
  | LPAREN e=expr RPAREN { e }
  | id=ID { VarExp (Var.of_int id) }