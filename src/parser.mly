%{
open Exp
%}

%token END
%token <int> INT
%token TRUE FALSE
%token LPAREN RPAREN
%token LT PLUS MINUS TIMES
%token IF THEN ELSE

%nonassoc prec_if
%left LT
%left PLUS MINUS
%left TIMES

%start toplevel
%type <Exp.exp> toplevel
%%

toplevel :
  | e=exp END { e }

exp :
  | IF c=exp THEN t=exp ELSE f=exp %prec prec_if { IfExp (c, t, f) }
  | l=exp LT r=exp { BOpExp (LtOp, l, r) }
  | l=exp PLUS r=exp { BOpExp (PlusOp, l, r) }
  | l=exp MINUS r=exp { BOpExp (MinusOp, l, r) }
  | l=exp TIMES r=exp { BOpExp (TimesOp, l, r) }
  | i=INT { IntExp i }
  | MINUS i=INT { IntExp (-i) }
  | TRUE { BoolExp true }
  | FALSE { BoolExp false }
  | LPAREN e=exp RPAREN { e }