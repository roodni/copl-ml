%{
open Expr
%}

%token END
%token <int> INT
%token TRUE FALSE
%token LPAREN RPAREN
%token LT PLUS MINUS TIMES
%token IF THEN ELSE
%token TURNSTILE
%token <Var.t> VAR
%token EQ COMMA
%token LET IN
%token FUN RIGHTARROW
%token LBRACKET RBRACKET
%token REC
%token <Loc.t> LOC
%token REF
%token ASSIGN
%token DEREF
%token SLASH
%token EVALTO

%nonassoc prec_let prec_fun prec_letrec
%nonassoc prec_if
%right ASSIGN
%left LT
%left PLUS MINUS
%left TIMES
%nonassoc INT TRUE FALSE LPAREN VAR DEREF

%start toplevel
%type <Toplevel.t> toplevel
%start loc_name
%type <Loc.t> loc_name
%%

toplevel :
  | e=expr END { Toplevel.create e Toplevel.Eval }
  | e=expr EVALTO { Toplevel.create e Toplevel.Judg }
  | en=env TURNSTILE ex=expr END { Toplevel.create ~env:en ex Toplevel.Eval }
  | en=env TURNSTILE ex=expr EVALTO { Toplevel.create ~env:en ex Toplevel.Judg }
  | s=store SLASH en=env TURNSTILE ex=expr END { Toplevel.create ~store:s ~env:en ex Toplevel.Eval }
  | s=store SLASH en=env TURNSTILE ex=expr EVALTO { Toplevel.create ~store:s ~env:en ex Toplevel.Judg }

store :
  | s=store_binds { let l, v = List.split s in Store.create l v }

store_binds :
  | { [] }
  | b=store_bind { [b] }
  | b=store_bind COMMA s=store_binds { b :: s }

store_bind :
  l=LOC EQ v=value  { (l, v) }

env :
  | { [] }
  | b=bind { [b] }
  | e=env COMMA b=bind { b :: e }

bind :
  | var=VAR EQ value=value { (var, value) }

value :
  | i=INT { Value.Int i }
  | TRUE { Value.Bool true }
  | FALSE { Value.Bool false }
  | LPAREN en=env RPAREN LBRACKET FUN v=VAR RIGHTARROW ex=expr RBRACKET
      { Value.Fun (en, v, ex) }
  | LPAREN en=env RPAREN LBRACKET REC f=VAR EQ FUN a=VAR RIGHTARROW ex=expr RBRACKET
      { Value.RecFun (en, f, a, ex) }
  | l=LOC { Value.Loc l }

expr :
  | IF c=expr THEN t=expr ELSE f=expr %prec prec_if { IfExp (c, t, f) }
  | l=expr LT r=expr { BOpExp (LtOp, l, r) }
  | l=expr PLUS r=expr { BOpExp (PlusOp, l, r) }
  | l=expr MINUS r=expr { BOpExp (MinusOp, l, r) }
  | l=expr TIMES r=expr { BOpExp (TimesOp, l, r) }
  | LET v=VAR EQ e1=expr IN e2=expr %prec prec_let { LetExp (v, e1, e2) }
  | FUN v=VAR RIGHTARROW e=expr %prec prec_fun { FunExp (v, e) }
  | l=expr r=simple { AppExp (l, r) }
  | e=simple { e }
  | LET REC f=VAR EQ FUN a=VAR RIGHTARROW e1=expr IN e2=expr %prec prec_letrec
      { LetRecExp (f, a, e1, e2) }
  | l=expr ASSIGN r=expr { BOpExp (AssignOp, l, r) }
  | REF e=simple { RefExp e }

simple :
  | i=INT { IntExp i }
  | TRUE { BoolExp true }
  | FALSE { BoolExp false }
  | LPAREN e=expr RPAREN { e }
  | v=VAR { VarExp v }
  | DEREF e=simple { DerefExp e }

loc_name :
  | value SLASH l=LOC EQ { l }
  | value COMMA l=LOC EQ { l }