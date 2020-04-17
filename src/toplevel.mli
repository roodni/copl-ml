type input_type = Judg | Eval

type t = {
  store : Store.t option;
  env : Value.env option;
  expr : Expr.t;
  input_type : input_type;
}

val create : ?store:Store.t -> ?env:Value.env -> Expr.t -> input_type -> t
