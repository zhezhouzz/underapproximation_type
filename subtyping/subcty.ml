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

let aux_sub_cty (axioms, uqvs) cty1 cty2 =
  (* let () = *)
  (*   Env.show_debug_queries @@ fun _ -> *)
  (*   Printf.printf "uqvs: %s\n" *)
  (*   @@ List.split_by_comma *)
  (*        (fun { x = ou, x; ty = cty } -> *)
  (*          spf "%s%s:(%s)" *)
  (*            (Normalty.Connective.qt_pretty_layout ou) *)
  (*            x (layout_cty cty)) *)
  (*        uqvs *)
  (* in *)
  let fa_ctx, ex_ctx = normalize_ctx uqvs in
  let nty, prop1, prop2 =
    match (cty1, cty2) with
    | Cty { nty = nty1; phi = phi1 }, Cty { nty = nty2; phi = phi2 } ->
        let nty = Nt._type_unify __FILE__ __LINE__ nty1 nty2 in
        (nty, phi1, phi2)
  in
  let () =
    Env.show_debug_queries @@ fun _ ->
    Printf.printf "prop1: %s\nprop2: %s\n" (layout_prop_ prop1)
      (layout_prop_ prop2)
  in
  let query = smart_implies prop1 prop2 in
  let query =
    List.fold_right (fun x body -> exists_cty_to_prop (x, body)) ex_ctx query
  in
  let query =
    List.fold_right (fun x body -> forall_cty_to_prop (x, body)) fa_ctx query
  in
  let query =
    match nty with
    | Nt.Ty_unit -> query
    | _ -> Forall { qv = default_v #: nty; body = query }
  in
  check_query axioms query

let aux_emptyness (axioms, uqvs) cty =
  let fa_ctx, ex_ctx = normalize_ctx uqvs in
  let nty, body = match cty with Cty { nty; phi } -> (nty, phi) in
  let body =
    match nty with
    | Nt.Ty_unit -> body
    | _ -> Exists { qv = default_v #: nty; body }
  in
  let query =
    List.fold_right
      (fun x cty -> exists_cty_to_prop (x, cty))
      (fa_ctx @ ex_ctx) body
  in
  (* let query = *)
  (*   List.fold_right (fun x body -> forall_cty_to_prop (x, body)) fa_ctx query *)
  (* in *)
  (* not (check_query axioms (Not query)) *)
  check_query axioms (Not query)

(* let rty_ctx_to_cty_ctx pctx = *)
(*   let rec aux (pctx : (t rty, string) typed list) uqvs = *)
(*     match List.last_destruct_opt pctx with *)
(*     | None -> uqvs *)
(*     | Some (pctx, binding) -> ( *)
(*         match binding.ty with *)
(*         | RtyInter _ -> _failatwith __FILE__ __LINE__ "unimp" *)
(*         | RtyGhostArr _ | RtyBaseDepPair _ | RtyBaseArr _ | RtyArrArr _ -> *)
(*             aux pctx uqvs *)
(*         | RtyBase { ou; cty } -> *)
(*             let x = (ou, binding.x) #: cty in *)
(*             aux pctx (x :: uqvs)) *)
(*   in *)
(*   match pctx with Typectx pctx -> aux pctx [] *)

let sub_cty pctx (cty1, cty2) =
  (* let () = pprint_typectx pctx.local_ctx in *)
  let ctx = rctx_to_cctx pctx.local_ctx in
  aux_sub_cty (pctx.axioms, ctx) cty1 cty2

let sub_cty_bool pctx (cty1, cty2) = sub_cty pctx (cty1, cty2)

let is_nonempty_cty pctx cty =
  let ctx = rctx_to_cctx pctx.local_ctx in
  aux_emptyness (pctx.axioms, ctx) cty

let is_nonempty_rty pctx rty =
  match rty with
  | RtyBase { ou = Ex; cty } -> is_nonempty_cty pctx cty
  | _ -> false
