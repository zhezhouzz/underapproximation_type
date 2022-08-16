open Quantified
open Languages.Qtypectx

let pretty_layout_raw (x : t) =
  Typectx.pretty_layout Underty.pretty_layout x.qbody

let pretty_layout (x : t) = layout_qt x.uqvs x.eqvs (pretty_layout_raw x)

let pretty_layout_judge ctx (e, (r : Languages.Qunderty.t)) =
  Typectx.pretty_layout_judge (pretty_layout ctx) (e, Qunderty.pretty_layout r)

let pretty_layout_subtyping ctx (r1, r2) =
  Typectx.pretty_layout_subtyping (pretty_layout ctx)
    (Qunderty.pretty_layout r1, Qunderty.pretty_layout r2)
