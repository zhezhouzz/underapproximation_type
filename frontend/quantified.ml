open Languages.Ntyped

type q = Fa | Ex

let layout_qt uqvs eqvs qbody =
  let mk_q (q, x, _, e) =
    let q = match q with Fa -> "∀" | Ex -> "∃" in
    Printf.sprintf "%s%s, %s" q x e
  in
  List.fold_right
    (fun { x; ty } e -> mk_q (Fa, x, ty, e))
    uqvs
    (List.fold_right (fun { x; ty } e -> mk_q (Ex, x, ty, e)) eqvs qbody)
