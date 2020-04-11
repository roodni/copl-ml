type input_type = Judg | Eval

type t = {
  store : Store.t option;
  env : Value.env option;
  expr : Expr.expr;
  input_type : input_type;
}

let create ?store ?env expr input_type = { store; env; expr; input_type }

let to_evaluatee t =
  {
    Evaluatee.store = Option.value t.store ~default:Store.empty;
    Evaluatee.env = Option.value t.env ~default:[];
    Evaluatee.expr = t.expr;
  }

let is_judg t = t.input_type = Judg
