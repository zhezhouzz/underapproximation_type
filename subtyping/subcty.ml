open Language
open Rctx
open Zzdatatype.Datatype
open Sugar
open Normalty.Connective

let layout_qt = function Fa -> "∀" | Ex -> "∃"

let layout_qv { x = qt, x; ty } =
  spf "%s%s:{%s}" (layout_qt qt) x @@ layout_cty ty

let layout_vs qt uqvs =
  List.split_by_comma layout_qv
  @@ List.map (fun { x; ty } -> { x = (qt, x); ty }) uqvs

(* let layout_prop_ = layout_prop_to_smtlib2 *)
let layout_prop_ = layout_prop_to_coq
(* let layout_prop_ = layout_prop *)

let rec normalize_ctx ctx =
  match ctx with
  | [] -> ([], [])
  | { x = Fa, x; ty = cty } :: ctx ->
      let fa_ctx, ex_ctx = normalize_ctx ctx in
      ((x #: cty) :: fa_ctx, ex_ctx)
  | { x = Ex, x; ty = cty } :: ctx ->
      let fa_ctx, ex_ctx = normalize_ctx ctx in
      (fa_ctx, (x #: cty) :: ex_ctx)

let rec remove_unit_in_ctx (ctx, prop) =
  match ctx with
  | [] -> ([], prop)
  | { x = Fa, _; ty = Cty { nty = Nt.Ty_unit; phi } } :: ctx ->
      let ctx, prop = remove_unit_in_ctx (ctx, prop) in
      (ctx, smart_implies phi prop)
  | { x = Ex, _; ty = Cty { nty = Nt.Ty_unit; phi } } :: ctx ->
      let ctx, prop = remove_unit_in_ctx (ctx, prop) in
      (ctx, smart_add_to phi prop)
  | x :: ctx ->
      let ctx, prop = remove_unit_in_ctx (ctx, prop) in
      (x :: ctx, prop)

let check_query axioms query =
  (* let query = Simp.peval_prop query in *)
  let () =
    Env.show_debug_queries @@ fun _ ->
    Printf.printf "query: %s\n" (layout_prop_ query)
  in
  (* let () = *)
  (*   Env.show_debug_queries @@ fun _ -> *)
  (*   Printf.printf "simpl query: %s\n" (layout_prop_ (Simp.peval_prop query)) *)
  (* in *)
  let fvs = fv_prop query in
  let () =
    _assert __FILE__ __LINE__
      (spf "the cty query has free variables %s"
         (List.split_by_comma
            (function { x; ty } -> spf "%s:%s" x (Nt.layout ty))
            fvs))
      (0 == List.length fvs)
  in
  Backend.Smtquery.check_bool axioms query

let aux_sub_prop (axioms, uqvs) prop1 prop2 =
  let () =
    Env.show_debug_queries @@ fun _ ->
    Printf.printf "prop1: %s\nprop2: %s\n" (layout_prop_ prop1)
      (layout_prop_ prop2)
  in
  let query = smart_implies prop1 prop2 in
  let uqvs, query = remove_unit_in_ctx (uqvs, query) in
  let fa_ctx, ex_ctx = normalize_ctx uqvs in
  let query =
    List.fold_right (fun x body -> exists_cty_to_prop (x, body)) ex_ctx query
  in
  let query =
    List.fold_right (fun x body -> forall_cty_to_prop (x, body)) fa_ctx query
  in
  check_query axioms query

let aux_nonemptyness (axioms, uqvs) cty =
  let fa_ctx, ex_ctx = normalize_ctx uqvs in
  let nty, body = match cty with Cty { nty; phi } -> (nty, phi) in
  let body =
    match nty with
    | Nt.Ty_unit -> body
    | _ -> Exists { qv = default_v #: nty; body }
  in
  let query =
    List.fold_right (fun x cty -> exists_cty_to_prop (x, cty)) ex_ctx body
  in
  let query =
    List.fold_right (fun x body -> forall_cty_to_prop (x, body)) fa_ctx query
  in
  (* not (check_query axioms (Not query)) *)
  check_query axioms query

let sub_prop pctx (phi1, phi2) =
  let () = pprint_typectx pctx.local_ctx in
  let ctx = rctx_to_cctx pctx.local_ctx in
  aux_sub_prop (pctx.axioms, ctx) phi1 phi2

let sub_cty pctx = function
  | Cty { nty = nty1; phi = phi1 }, Cty { nty = nty2; phi = phi2 } ->
      let nty = Nt._type_unify __FILE__ __LINE__ nty1 nty2 in
      let () = pprint_typectx pctx.local_ctx in
      let ctx = rctx_to_cctx pctx.local_ctx in
      let binding =
        match nty with
        | Nt.Ty_unit -> []
        | _ -> [ (Fa, default_v) #: (prop_to_cty nty mk_true) ]
      in
      let ctx = ctx @ binding in
      aux_sub_prop (pctx.axioms, ctx) phi1 phi2

let sub_cty_bool pctx (cty1, cty2) = sub_cty pctx (cty1, cty2)

let is_nonempty_cty pctx cty =
  let ctx = rctx_to_cctx pctx.local_ctx in
  aux_nonemptyness (pctx.axioms, ctx) cty

let is_nonempty_cty_opt pctx cty =
  let ctx = rctx_to_cctx pctx.local_ctx in
  if aux_nonemptyness (pctx.axioms, ctx) cty then Some cty else None
