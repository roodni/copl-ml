type input_type = Judg | Eval

type t = {
  store : Store.t option;
  env : Value.env option;
  expr : Expr.t;
  input_type : input_type;
}

let create ?store ?env expr input_type = { store; env; expr; input_type }
