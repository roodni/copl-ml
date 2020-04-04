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
%token <string> ID
%token EQ COMMA
%token LET IN
%token FUN RIGHTARROW

%nonassoc prec_let prec_fun
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
  | ex=expr END { {env = []; expr = ex} }

env :
  | { [] }
  | a=assign { [a] }
  | e=env COMMA a=assign { a :: e }

assign :
  | id=ID EQ value=value { (Var.of_string id, value) }

value :
  | i=INT { IntVal i }
  | MINUS i=INT { IntVal ~-i }
  | TRUE { BoolVal true }
  | FALSE { BoolVal false }

expr :
  | IF c=expr THEN t=expr ELSE f=expr %prec prec_if { IfExp (c, t, f) }
  | l=expr LT r=expr { BOpExp (LtOp, l, r) }
  | l=expr PLUS r=expr { BOpExp (PlusOp, l, r) }
  | l=expr MINUS r=expr { BOpExp (MinusOp, l, r) }
  | l=expr TIMES r=expr { BOpExp (TimesOp, l, r) }
  | LET id=ID EQ e1=expr IN e2=expr %prec prec_let { LetExp (Var.of_string id, e1, e2) }
  | FUN id=ID RIGHTARROW e=expr %prec prec_fun { FunExp (Var.of_string id, e) }
  | l=expr r=simple { AppExp (l, r) }
  | e=simple { e }

%inline simple :
  | i=INT { IntExp i }
  | TRUE { BoolExp true }
  | FALSE { BoolExp false }
  | LPAREN e=expr RPAREN { e }
  | id=ID { VarExp (Var.of_string id) }