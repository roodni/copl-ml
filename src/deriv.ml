open Syntax

type value = IntVal of int | BoolVal of bool

let value_to_string = function
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b

type rule =
  | EInt
  | EBool
  | EIfT
  | EIfF
  | EPlus
  | EMinus
  | ETimes
  | ELt
  | BPlus
  | BMinus
  | BTimes
  | BLt

let rule_to_string = function
  | EInt -> "E-Int"
  | EBool -> "E-Bool"
  | EIfT -> "E-IfT"
  | EIfF -> "E-IfF"
  | EPlus -> "E-Plus"
  | EMinus -> "E-Minus"
  | ETimes -> "E-Times"
  | ELt -> "E-Lt"
  | BPlus -> "B-Plus"
  | BMinus -> "B-Minus"
  | BTimes -> "B-Times"
  | BLt -> "B-Lt"

type judgment =
  | EvalJ of { exp : exp; value : value }
  | PlusJ of int * int * int
  | MinusJ of int * int * int
  | TimesJ of int * int * int
  | LtJ of int * int * bool

let judgment_to_string = function
  | EvalJ { exp; value } ->
      Printf.sprintf "%s evalto %s" (exp_to_string exp) (value_to_string value)
  | PlusJ (l, r, s) -> Printf.sprintf "%d plus %d is %d" l r s
  | MinusJ (l, r, d) -> Printf.sprintf "%d minus %d is %d" l r d
  | TimesJ (l, r, p) -> Printf.sprintf "%d times %d is %d" l r p
  | LtJ (l, r, b) -> Printf.sprintf "%d less than %d is %b" l r b

type deriv = { concl : judgment; rule : rule; premises : deriv list }

let rec output_deriv ?(indent = 0) ?(outchan = stdout) { premises; rule; concl }
    =
  let printf f = Printf.fprintf outchan f in
  let rec output_indent depth =
    if depth > 0 then (
      printf "  ";
      output_indent (depth - 1) )
  in
  output_indent indent;
  printf "%s by %s " (judgment_to_string concl) (rule_to_string rule);
  if premises = [] then printf "{};\n"
  else (
    printf "{\n";
    List.iter
      (fun deriv -> output_deriv ~indent:(indent + 1) ~outchan deriv)
      premises;
    output_indent indent;
    printf "};\n" )

exception EvalError of string

let rec eval_exp_to_deriv exp =
  match exp with
  | IntExp i ->
      let value = IntVal i in
      (value, { concl = EvalJ { exp; value }; rule = EInt; premises = [] })
  | BoolExp b ->
      let value = BoolVal b in
      (value, { concl = EvalJ { exp; value }; rule = EBool; premises = [] })
  | IfExp (c, t, f) ->
      let cvalue, cderiv = eval_exp_to_deriv c in
      let retexp, rule =
        match cvalue with
        | BoolVal true -> (t, EIfT)
        | BoolVal false -> (f, EIfF)
        | _ -> raise (EvalError "condition must be boolean: if")
      in
      let retvalue, retderiv = eval_exp_to_deriv retexp in
      ( retvalue,
        {
          concl = EvalJ { exp; value = retvalue };
          rule;
          premises = [ cderiv; retderiv ];
        } )
  | BOpExp (((PlusOp | MinusOp | TimesOp | LtOp) as op), lexp, rexp) -> (
      let lvalue, lderiv = eval_exp_to_deriv lexp
      and rvalue, rderiv = eval_exp_to_deriv rexp in
      match (lvalue, rvalue) with
      | IntVal li, IntVal ri ->
          let value, erule, bjudg, brule =
            match op with
            | PlusOp ->
                let i = li + ri in
                (IntVal i, EPlus, PlusJ (li, ri, i), BPlus)
            | MinusOp ->
                let i = li - ri in
                (IntVal i, EMinus, MinusJ (li, ri, i), BMinus)
            | TimesOp ->
                let i = li * ri in
                (IntVal i, ETimes, TimesJ (li, ri, i), BTimes)
            | LtOp ->
                let b = li < ri in
                (BoolVal b, ELt, LtJ (li, ri, b), BLt)
          in
          ( value,
            {
              concl = EvalJ { exp; value };
              rule = erule;
              premises =
                [
                  lderiv; rderiv; { concl = bjudg; rule = brule; premises = [] };
                ];
            } )
      | _ ->
          raise
            (EvalError ("both arguments must be integer: " ^ binop_to_string op))
      )
