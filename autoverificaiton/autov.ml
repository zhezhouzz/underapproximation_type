module Prop = Prop.T
module Smtty = Smtty.T

let ctx =
  Z3.mk_context [ ("model", "true"); ("proof", "false"); ("timeout", "1999") ]

let pretty_print_model model =
  Z3.Model.to_string model |> fun s ->
  List.fold_left
    (fun acc c ->
      Str.global_replace
        (Str.regexp
           ("\\([^ ] \\)"
           ^ (Z3.Model.get_const_interp model c
             |> Option.get |> Z3.Expr.to_string)))
        ("\\1" ^ (Z3.FuncDecl.get_name c |> Z3.Symbol.to_string))
        acc)
    s
    (Z3.Model.get_const_decls model)
  |> Printf.printf "%s\n"

let _check q =
  let open Check in
  match smt_neg_and_solve ctx q with
  | SmtUnsat -> true
  | SmtSat model ->
      Printf.printf "model:\n%s\n" @@ Z3.Model.to_string model;
      (* pretty_print_model model; *)
      false
  | Timeout -> failwith "smt timeout"

let check vc = _check vc
let check_implies a b = _check Prop.(Implies (a, b))
let check_implies_multi_pre a_s b = _check Prop.(Implies (And a_s, b))
let prop_of_ocamlexpr = Frontend.prop_of_ocamlexpr
let prop_to_ocamlexpr = Frontend.prop_to_expr
let layout_prop = Frontend.layout
let pretty_layout_prop = Frontend.pretty_layout
let pretty_layout_lit = Frontend.pretty_layout_lit
let coq_layout_prop = Frontend.coq_layout
let prop_fv = Fv.fv
let add_prop_to_fv = Fv.add_fv
