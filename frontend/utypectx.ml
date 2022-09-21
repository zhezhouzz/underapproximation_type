open Ast
open UnderTypectx
open Zzdatatype.Datatype

let pretty_layout_raw (x : t) = Typectx.pretty_layout Underty.pretty_layout x
let pretty_layout (x : t) = pretty_layout_raw x

let pretty_print (x : t) =
  Typectx.pretty_print Underty.pretty_layout x;
  print_newline ()

let pretty_layout_judge ctx (e, (r : UT.t)) =
  Typectx.pretty_layout_judge (pretty_layout ctx) (e, Underty.pretty_layout r)

let pretty_print_judge ctx (e, (r : UT.t)) =
  let () = Pp.printf "@{<bold>Type Check:@}\n" in
  pretty_print ctx;
  Pp.printf "⊢ @{<magenta>%s@} ⇦ " e;
  Pp.printf "@{<cyan>%s@}\n\n" @@ Underty.pretty_layout r

let pretty_print_app_judge ctx (args, (r : UT.t)) =
  let () = Pp.printf "@{<bold>Application Type Check:@}\n" in
  pretty_print ctx;
  Pp.printf "⊢ @{<magenta>%s → ? @} ⇦ "
    (List.split_by " → " (fun x -> x.UL.x) args);
  Pp.printf "@{<cyan>%s@}\n\n" @@ Underty.pretty_layout r

let pretty_print_infer ctx (e, (r : UT.t)) =
  let () = Pp.printf "@{<bold>Type Infer:@}\n" in
  pretty_print ctx;
  Pp.printf "⊢ @{<magenta>%s@} ⇨ " e;
  Pp.printf "@{<cyan>%s@}\n\n" @@ Underty.pretty_layout r

let pretty_layout_subtyping ctx (r1, r2) =
  Typectx.pretty_layout_subtyping (pretty_layout ctx)
    (Underty.pretty_layout r1, Underty.pretty_layout r2)

let pretty_print_subtyping ctx (r1, r2) =
  let () = Pp.printf "@{<bold>Subtyping Check:@}\n" in
  pretty_print ctx;
  Pp.printf "⊢ @{<magenta>%s@} <: @{<cyan>%s@}\n\n" (Underty.pretty_layout r1)
    (Underty.pretty_layout r2)

let pretty_print_q uqvs eqvs pre body =
  let () = Pp.printf "@{<bold>Query:@}\n" in
  Quantified.print_qt_ uqvs eqvs;
  Pp.printf "\n@{<cyan>%s@} @{<bold>=>@}\n@{<magenta>%s@}\n"
    (Autov.coq_layout_prop pre)
    (Autov.coq_layout_prop body)
