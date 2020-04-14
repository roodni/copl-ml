open Printf

module type SYSTEM = sig
  type rule

  type judgment

  val rule_to_string : rule -> string

  val judgment_to_string : judgment -> string
end

module Make (Sy : SYSTEM) = struct
  type t = { concl : Sy.judgment; rule : Sy.rule; premises : t list }

  let rec output ?(indent = 0) ?(outchan = stdout) { premises; rule; concl } =
    let printf f = fprintf outchan f in
    let rec output_indent depth =
      if depth > 0 then (
        printf "  ";
        output_indent (depth - 1) )
    in
    output_indent indent;
    printf "%s by %s " (Sy.judgment_to_string concl) (Sy.rule_to_string rule);
    if premises = [] then printf "{};\n"
    else (
      printf "{\n";
      List.iter
        (fun deriv -> output ~indent:(indent + 1) ~outchan deriv)
        premises;
      output_indent indent;
      printf "};\n" )
end
