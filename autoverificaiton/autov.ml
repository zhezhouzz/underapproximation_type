module Prop = Prop.T
module Smtty = Smtty.T

let ctx =
  Z3.mk_context [ ("model", "true"); ("proof", "false"); ("timeout", "1999") ]

let _check q =
  let open Check in
  match smt_solve ctx q with
  | SmtUnsat -> true
  | SmtSat model ->
      Printf.printf "%s\n" @@ Z3.Model.to_string model;
      false
  | Timeout -> failwith "smt timeout"

let check vc = _check (Prop.Not vc)
let check_implies a b = _check Prop.(Not (Implies (a, b)))
let check_implies_multi_pre a_s b = _check Prop.(Not (Implies (And a_s, b)))
let prop_of_ocamlexpr = Frontend.prop_of_ocamlexpr
let prop_to_ocamlexpr = Frontend.prop_to_expr
let layout_prop = Frontend.layout
let pretty_layout_prop = Frontend.pretty_layout
let prop_fv = Fv.fv
let add_prop_to_fv = Fv.add_fv
