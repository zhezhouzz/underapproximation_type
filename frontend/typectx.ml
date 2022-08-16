open Zzdatatype.Datatype

let pretty_layout f ctx =
  List.split_by ";\n"
    (fun (name, ty) -> Printf.sprintf "%s:%s" name (List.split_by "/\\" f ty))
    ctx

let pretty_layout_over = pretty_layout Overty.pretty_layout
let pretty_layout_judge ctx (e, ty) = Printf.sprintf "%s⊢\n%s :\n%s\n" ctx e ty

let pretty_layout_over_judge ctx (e, r) =
  pretty_layout_judge (pretty_layout_over ctx) (e, Overty.pretty_layout r)

(* let pretty_layout_under_judge ctx (e, (r : Languages.Qunderty.t)) = *)
(*   pretty_layout_judge (pretty_layout_under ctx) (e, Qunderty.pretty_layout r) *)

let pretty_layout_subtyping ctx (r1, r2) =
  Printf.sprintf "%s⊢\n%s <:\n%s\n" ctx r1 r2

let pretty_layout_over_subtyping ctx (r1, r2) =
  pretty_layout_subtyping
    (pretty_layout Overty.pretty_layout ctx)
    (Overty.pretty_layout r1, Overty.pretty_layout r2)

let pretty_layout_under_subtyping ctx (r1, r2) =
  pretty_layout_subtyping
    (pretty_layout Underty.pretty_layout ctx)
    (Underty.pretty_layout r1, Underty.pretty_layout r2)
