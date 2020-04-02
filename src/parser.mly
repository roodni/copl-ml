%{
open Exp
open Value
open Evaluatee
open Var
%}

%token END
%token <int> INT
%token TRUE FALSE
%token LPAREN RPAREN
%token LT PLUS MINUS TIMES
%token IF THEN ELSE
%token TURNSTILE
%token <string> ID
%token EQ COMMA

%nonassoc prec_if
%left LT
%left PLUS MINUS
%left TIMES

%start toplevel
%type <Evaluatee.evaluatee> toplevel
%%

toplevel :
  | en=env TURNSTILE ex=exp END { {env = en; exp = ex} }
  | ex=exp END { {env = []; exp = ex} }

env :
  | { [] }
  | a=assign { [a] }
  | e=env COMMA a=assign { a :: e }

assign :
  | id=ID EQ value=value { (Var id, value) }

value :
  | i=INT { IntVal i }
  | MINUS i=INT { IntVal ~-i }
  | TRUE { BoolVal true }
  | FALSE { BoolVal false }

exp :
  | IF c=exp THEN t=exp ELSE f=exp %prec prec_if { IfExp (c, t, f) }
  | l=exp LT r=exp { BOpExp (LtOp, l, r) }
  | l=exp PLUS r=exp { BOpExp (PlusOp, l, r) }
  | l=exp MINUS r=exp { BOpExp (MinusOp, l, r) }
  | l=exp TIMES r=exp { BOpExp (TimesOp, l, r) }
  | i=INT { IntExp i }
  | MINUS i=INT { IntExp ~-i }
  | TRUE { BoolExp true }
  | FALSE { BoolExp false }
  | LPAREN e=exp RPAREN { e }
  | id=ID { VarExp (Var id) }