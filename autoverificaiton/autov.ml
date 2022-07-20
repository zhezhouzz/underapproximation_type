module Prop = Prop.Prop

let ctx =
  Z3.mk_context [ ("model", "true"); ("proof", "false"); ("timeout", "1999") ]

let check vc = Check.check ctx vc
let prop_of_ocamlexpr = Frontend.prop_of_ocamlexpr
let prop_to_ocamlexpr = Frontend.prop_to_expr
let layout_prop = Frontend.layout
