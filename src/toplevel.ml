type input_type = Judg | Eval

type t = {
  store : Store.t option;
  env : Value.env option;
  expr : Expr.t;
  input_type : input_type;
}

let create ?store ?env expr input_type = { store; env; expr; input_type }

let to_evalee t =
  {
    Evalee.store = Option.value t.store ~default:Store.empty;
    Evalee.env = Option.value t.env ~default:[];
    Evalee.expr = t.expr;
  }

let is_judg t = t.input_type = Judg
