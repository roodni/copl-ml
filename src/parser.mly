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
%token REF ASSIGN DEREF
%token SLASH
%token EVALTO
%token CONS NIL
%token MATCH WITH BAR
%token COLON
%token INTT BOOLT LISTT
%token <string> TVAR
%token DOT

%right RIGHTARROW
%nonassoc LISTT

%nonassoc prec_let prec_fun prec_letrec prec_match
%nonassoc BAR
%nonassoc prec_if
%right ASSIGN
%left LT
%right CONS
%left PLUS MINUS
%left TIMES
%nonassoc INT TRUE FALSE LPAREN VAR DEREF NIL

%start toplevel
%type <Toplevel.t> toplevel
%start loc_name
%type <Loc.t> loc_name
%start types_expected
%type <Types.t option> types_expected
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
  l=LOC EQ v=value  { (l, v) }

env :
  | { [] }
  | e=env_not_empty { e }

env_not_empty :
  | b=bind { [b] }
  | e=env_not_empty COMMA b=bind { b :: e }

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
  | NIL { Value.Nil }
  | l=value CONS r=value { Value.Cons (l, r) }
  | LPAREN v=value RPAREN { v }

expr :
  | IF c=expr THEN t=expr ELSE f=expr %prec prec_if { Expr.If (c, t, f) }
  | l=expr LT r=expr { Expr.BOp (LtOp, l, r) }
  | l=expr PLUS r=expr { Expr.BOp (PlusOp, l, r) }
  | l=expr MINUS r=expr { Expr.BOp (MinusOp, l, r) }
  | l=expr TIMES r=expr { Expr.BOp (TimesOp, l, r) }
  | LET v=VAR EQ e1=expr IN e2=expr %prec prec_let { Expr.Let (v, e1, e2) }
  | FUN v=VAR RIGHTARROW e=expr %prec prec_fun { Expr.Fun (v, e) }
  | l=expr r=simple { Expr.App (l, r) }
  | e=simple { e }
  | LET REC f=VAR EQ FUN a=VAR RIGHTARROW e1=expr IN e2=expr %prec prec_letrec
      { Expr.LetRec (f, a, e1, e2) }
  | l=expr ASSIGN r=expr { Expr.BOp (AssignOp, l, r) }
  | REF e=simple { Expr.Ref e }
  | l=expr CONS r=expr { Expr.BOp (ConsOp, l, r) }
  | MATCH e=expr WITH c=clauses { Expr.Match (e, c) }

simple :
  | i=INT { Expr.Int i }
  | TRUE { Expr.Bool true }
  | FALSE { Expr.Bool false }
  | LPAREN e=expr RPAREN { e }
  | v=VAR { Expr.Var v }
  | DEREF e=simple { Expr.Deref e }
  | NIL { Expr.Nil }

clauses :
  | p=pat RIGHTARROW e=expr %prec prec_match { [ (p, e) ] }
  | p=pat RIGHTARROW e=expr BAR c=clauses { (p, e) :: c }

pat :
  | v=VAR { if Var.to_string v = "_" then Expr.WildPat else Expr.VarPat v }
  | NIL { Expr.NilPat }
  | l=pat CONS r=pat { Expr.ConsPat (l, r) }
  | LPAREN p=pat RPAREN { p }

loc_name :
  | value SLASH l=LOC EQ { l }
  | value COMMA l=LOC EQ { l }

type_env_not_empty :
  | b=type_bind { [b] }
  | e=type_env_not_empty COMMA b=type_bind { b :: e }

type_bind :
  | v=VAR COLON s=tyscheme { (v, s) }

tyscheme :
  | t=types { Tscheme.create [] t }
  | l=tvars DOT t=types { Tscheme.create l t }

tvars :
  | v=tvar { [v] }
  | v=tvar l=tvars { v :: l }

types :
  | INTT { Types.Int }
  | BOOLT { Types.Bool }
  | t=types LISTT { Types.List t }
  | t1=types RIGHTARROW t2=types { Types.Fun (t1, t2) }
  | LPAREN t=types RPAREN { t }
  | v=tvar { Types.Var v }

tvar :
  | v=TVAR { Tvar.of_name v }

types_expected :
  | END { None }
  | t=types END { Some t }