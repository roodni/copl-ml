open Syntax

type value = IntVal of int | BoolVal of bool

let value_to_string = function
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b

type evalrule = EInt | EBool | EIfT | EIfF | EPlus | EMinus | ETimes | ELt

let evalrule_to_string = function
  | EInt -> "E-Int"
  | EBool -> "E-Bool"
  | EIfT -> "E-IfT"
  | EIfF -> "E-IfF"
  | EPlus -> "E-Plus"
  | EMinus -> "E-Minus"
  | ETimes -> "E-Times"
  | ELt -> "E-Lt"

type concl =
  | Eval of { exp : exp; value : value; rule : evalrule }
  | BPlus of int * int * int
  | BMinus of int * int * int
  | BTimes of int * int * int
  | BLt of int * int * bool

let concl_to_string = function
  | Eval { exp; value; rule } ->
      Printf.sprintf "%s evalto %s by %s" (exp_to_string exp)
        (value_to_string value) (evalrule_to_string rule)
  | BPlus (l, r, s) -> Printf.sprintf "%d plus %d is %d by B-Plus" l r s
  | BMinus (l, r, d) -> Printf.sprintf "%d minus %d is %d by B-Minus" l r d
  | BTimes (l, r, p) -> Printf.sprintf "%d times %d is %d by B-Times" l r p
  | BLt (l, r, b) -> Printf.sprintf "%d less than %d is %b by B-Lt" l r b

type deriv = { premises : deriv list; concl : concl }

let rec output_deriv ?(indent = 0) ?(outchan = stdout) { premises; concl } =
  let printf f = Printf.fprintf outchan f in
  let rec output_indent depth =
    if depth > 0 then (
      printf "  ";
      output_indent (depth - 1) )
  in
  output_indent indent;
  printf "%s {" (concl_to_string concl);
  if premises <> [] then (
    printf "\n";
    List.iter
      (fun deriv -> output_deriv ~indent:(indent + 1) ~outchan deriv)
      premises;
    output_indent indent );
  printf "};\n"

exception EvalError of string

let rec eval_exp_to_deriv exp =
  match exp with
  | IntExp i ->
      let value = IntVal i in
      (value, { premises = []; concl = Eval { exp; value; rule = EInt } })
  | BoolExp b ->
      let value = BoolVal b in
      (value, { premises = []; concl = Eval { exp; value; rule = EBool } })
  | IfExp (c, t, f) ->
      let cvalue, cderiv = eval_exp_to_deriv c in
      let retexp, rule =
        match cvalue with
        | BoolVal true -> (t, EIfT)
        | BoolVal false -> (f, EIfF)
        | _ -> raise (EvalError "conditional expression must be boolean: if")
      in
      let retvalue, retderiv = eval_exp_to_deriv retexp in
      ( retvalue,
        {
          premises = [ cderiv; retderiv ];
          concl = Eval { exp; value = retvalue; rule };
        } )
  | BOpExp (((PlusOp | MinusOp | TimesOp | LtOp) as op), lexp, rexp) -> (
      let lvalue, lderiv = eval_exp_to_deriv lexp
      and rvalue, rderiv = eval_exp_to_deriv rexp in
      match (lvalue, rvalue) with
      | IntVal li, IntVal ri ->
          let value, bpremise, rule =
            match op with
            | PlusOp ->
                let i = li + ri in
                (IntVal i, BPlus (li, ri, i), EPlus)
            | MinusOp ->
                let i = li - ri in
                (IntVal i, BMinus (li, ri, i), EMinus)
            | TimesOp ->
                let i = li * ri in
                (IntVal i, BTimes (li, ri, i), ETimes)
            | LtOp ->
                let b = li < ri in
                (BoolVal b, BLt (li, ri, b), ELt)
          in
          ( value,
            {
              premises = [ lderiv; rderiv; { premises = []; concl = bpremise } ];
              concl = Eval { exp; value; rule };
            } )
      | _ ->
          raise
            (EvalError ("both arguments must be integer: " ^ binop_to_string op))
      )
