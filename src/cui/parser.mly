%{
open Base
open Evalml
open Table
%}

%token END
%token <int> INT
%token TRUE FALSE
%token LPAREN RPAREN
%token LT PLUS MINUS TIMES
%token IF THEN ELSE
%token TURNSTILE
%token <string> VAR
%token EQ COMMA
%token LET IN
%token FUN RIGHTARROW
%token LBRACKET RBRACKET
%token REC
%token <string> LOC
%token REF ASSIGN DEREF
%token SLASH
%token EVALTO
%token CONS NIL
%token MATCH WITH BAR
%token COLON
%token INTT BOOLT LISTT
%token <string> TVAR
%token DOT
%token UNDER
%token GTGT
%token LBRACE RBRACE
%token LETCC

%right RIGHTARROW
%nonassoc LISTT

%nonassoc prec_let prec_fun prec_letrec prec_match prec_letcc
%nonassoc BAR
%nonassoc prec_if
%right ASSIGN
%left LT
%right CONS
%left PLUS MINUS
%left TIMES
%nonassoc INT TRUE FALSE LPAREN VAR DEREF NIL UNDER

%start toplevel
%type <Toplevel.t> toplevel

%start loc_name
%type <Evalml.Loc.t> loc_name

%start types_expected
%type <Typingml.Types.t option> types_expected

%start toplevel_cont
%type <Toplevel.t> toplevel_cont
%%

toplevel :
  | e=expr is_judg=eval_end { Toplevel.create_eval e ~is_judg }
  | en=env_not_empty TURNSTILE ex=expr is_judg=eval_end
      { Toplevel.create_eval ~env:en ex ~is_judg }
  | s=store SLASH en=env TURNSTILE ex=expr is_judg=eval_end
      { Toplevel.create_eval ~store:s ~env:en ex ~is_judg }
  | t=type_env_not_empty TURNSTILE e=expr COLON
      { Toplevel.Typing { tenv = t; expr = e } }
  | TURNSTILE e=expr is_judg=eval_end
      { Toplevel.create_eval ~env:[] e ~is_judg }
  | TURNSTILE e=expr COLON
      { Toplevel.Typing { tenv = []; expr = e } }

eval_end :
  | END { false }
  | EVALTO { true }


store :
  | s=store_binds { let l, v = List.split s in Store.create l v }

store_binds :
  | { [] }
  | b=store_bind { [b] }
  | b=store_bind COMMA s=store_binds { b :: s }

store_bind :
  | l=loc EQ v=value  { (l, v) }

loc :
  | l=LOC { Loc.of_string l }

loc_name :
  | value SLASH l=loc EQ { l }
  | value COMMA l=loc EQ { l }


env :
  | { [] }
  | e=env_not_empty { e }

env_not_empty :
  | b=bind { [b] }
  | e=env_not_empty COMMA b=bind { b :: e }

bind :
  | var=var EQ value=value { (var, value) }

var :
  | v=VAR { Var.of_string v }
  | UNDER { Var.of_string "_" }

value :
  | i=INT { Value.Int i }
  | TRUE { Value.Bool true }
  | FALSE { Value.Bool false }
  | LPAREN en=env RPAREN LBRACKET FUN v=var RIGHTARROW ex=expr RBRACKET
      { Value.Fun (en, v, ex) }
  | LPAREN en=env RPAREN LBRACKET REC f=var EQ FUN a=var RIGHTARROW ex=expr RBRACKET
      { Value.RecFun (en, f, a, ex) }
  | l=loc { Value.Loc l }
  | NIL { Value.Nil }
  | l=value CONS r=value { Value.Cons (l, r) }
  | LPAREN v=value RPAREN { v }
  | LBRACKET k=cont RBRACKET { Value.Cont k }


expr :
  | IF c=expr THEN t=expr ELSE f=expr %prec prec_if { Expr.If (c, t, f) }
  | l=expr op=bop r=expr { Expr.BOp (op, l, r) }
  | LET v=var EQ e1=expr IN e2=expr %prec prec_let { Expr.Let (v, e1, e2) }
  | FUN v=var RIGHTARROW e=expr %prec prec_fun { Expr.Fun (v, e) }
  | l=expr r=simple { Expr.App (l, r) }
  | e=simple { e }
  | LET REC f=var EQ FUN a=var RIGHTARROW e1=expr IN e2=expr %prec prec_letrec
      { Expr.LetRec (f, a, e1, e2) }
  | l=expr ASSIGN r=expr { Expr.Assign (l, r) }
  | REF e=simple { Expr.Ref e }
  | l=expr CONS r=expr { Expr.Cons (l, r) }
  | MATCH e=expr WITH c=clauses { Expr.Match (e, c) }
  | LETCC v=var IN e=expr %prec prec_letcc { Expr.Letcc (v, e) }

%inline bop :
  | LT { Expr.LtOp }
  | PLUS { Expr.PlusOp }
  | MINUS { Expr.MinusOp }
  | TIMES { Expr.TimesOp }

simple :
  | i=INT { Expr.Int i }
  | TRUE { Expr.Bool true }
  | FALSE { Expr.Bool false }
  | LPAREN e=expr RPAREN { e }
  | v=var { Expr.Var v }
  | DEREF e=simple { Expr.Deref e }
  | NIL { Expr.Nil }


clauses :
  | p=pat RIGHTARROW e=expr %prec prec_match { [ (p, e) ] }
  | p=pat RIGHTARROW e=expr BAR c=clauses { (p, e) :: c }

pat :
  | UNDER { Expr.WildPat }
  | v=VAR { Expr.VarPat (Var.of_string v) }
  | NIL { Expr.NilPat }
  | l=pat CONS r=pat { Expr.ConsPat (l, r) }
  | LPAREN p=pat RPAREN { p }


type_env_not_empty :
  | b=type_bind { [b] }
  | e=type_env_not_empty COMMA b=type_bind { b :: e }

type_bind :
  | v=var COLON s=tyscheme { (v, s) }

tyscheme :
  | t=types { Typingml.Tscheme.create [] t }
  | l=tvars DOT t=types { Typingml.Tscheme.create l t }

tvars :
  | v=tvar { [v] }
  | v=tvar l=tvars { v :: l }

types :
  | INTT { Typingml.Types.Int }
  | BOOLT { Typingml.Types.Bool }
  | t=types LISTT { Typingml.Types.List t }
  | t1=types RIGHTARROW t2=types { Typingml.Types.Fun (t1, t2) }
  | LPAREN t=types RPAREN { t }
  | v=tvar { Typingml.Types.Var v }

tvar :
  | v=TVAR { Typingml.Tvar.of_name v }

types_expected :
  | END { None }
  | t=types END { Some t }


toplevel_cont :
  | env=env_optional e=expr eval_end { Toplevel.create_cont ?env e [] }
  | env=env_optional e=expr GTGT c=cont eval_end { Toplevel.create_cont ?env e c }

cont :
  | UNDER { [] }
  | u=cont_unit { [u] }
  | u=cont_unit GTGT c=cont { u :: c }

cont_unit :
  | LBRACE u=cont_braced RBRACE { u }

cont_braced :
  | env=env_optional UNDER op=bop e=expr { Cont.BOpL (op, env, e) }
  | v=value op=bop UNDER { Cont.BOpR (op, v) }
  | env=env_optional IF UNDER THEN e1=expr ELSE e2=expr { Cont.If (env, e1, e2) }
  | env=env TURNSTILE LET v=var EQ UNDER IN e=expr { Cont.Let (env, v, e) }
  | env=env TURNSTILE UNDER e=expr { Cont.AppL (env, e) }
  | v=value UNDER { Cont.AppR v }
  | env=env TURNSTILE UNDER CONS e=expr { Cont.ConsL (env, e) }
  | v=value CONS UNDER { Cont.ConsR v }
  | env=env TURNSTILE MATCH UNDER WITH NIL RIGHTARROW e1=expr BAR x=var CONS y=var RIGHTARROW e2=expr
      {Cont.Match (env, e1, x, y, e2) }

%inline env_optional :
  | { None }
  | env=env TURNSTILE { Some env }