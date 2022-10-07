open Languages
module P = Autov.Prop
module Typectx = MustMayTypectx
open Zzdatatype.Datatype
open Sugar
open Ntyped
open Abstraction

let with_lemma_to_query lemmas x =
  let pre, a, b = Lemma.query_with_lemma_to_prop @@ Lemma.with_lemma lemmas x in
  (* let () = Lemma.print_with_lemma (pre, b) in *)
  (pre, a, b)

let typed_to_smttyped = Languages.Ntyped.to_smttyped

let check file line pres q =
  match Autov.check pres q with
  | None -> ()
  | Some m ->
      (* let _ = Autov.Func_interp.get_preds_interp m in *)
      Autov._failwithmodel file line "Subtyping check: rejected by the verifier"
        m

let do_check file line (final_uqvs, final_eqvs, final_pre, final_post) =
  let () =
    let hol_q =
      P.(
        topu_to_prop
          ( final_uqvs,
            tope_to_prop (final_eqvs, Implies (final_pre, final_post)) ))
    in
    match P.fv hol_q with
    | [] -> ()
    | fvs ->
        (* let () = Printf.printf "Q: %s\n" (Autov.pretty_layout_prop hol_q) in *)
        _failatwith __FILE__ __LINE__ @@ StrList.to_string fvs
  in
  let pres, uqvs, q =
    with_lemma_to_query (Prim.lemmas_to_pres ())
      (final_uqvs, final_eqvs, final_pre, final_post)
  in
  match
    List.substract String.equal (Autov.prop_fv q) (List.map (fun x -> x.x) uqvs)
  with
  | [] -> check file line pres q
  | fv ->
      let () = Printf.printf "q: %s\n" @@ Autov.pretty_layout_prop q in
      _failatwith __FILE__ __LINE__
        (spf "FV: %s" @@ Zzdatatype.Datatype.StrList.to_string fv)

(* let counter = ref 0 *)

let check_under_ctx file line ctx (t1, t2) =
  let solve pres =
    let rec aux = function
      | (id, ot) :: pres ->
          if NT.is_basic_tp ot.UT.normalty then
            let x = { x = id; ty = ot.UT.normalty } in
            let prop = P.subst_id ot.prop ot.basename x.x in
            let ot_uqvs, ot_pre = aux pres in
            (x :: ot_uqvs, P.And [ prop; ot_pre ])
          else aux pres
      | [] -> ([], P.mk_true)
    in
    let ot_uqvs, ot_pre = aux pres in
    (ot_uqvs, P.peval ot_pre)
  in
  let solve_pres pres (t1, t2) =
    let name1, nt1, prop1 = UT.assume_base __FILE__ __LINE__ t1 in
    let name2, nt2, prop2 = UT.assume_base __FILE__ __LINE__ t2 in
    let nt = _check_equality __FILE__ __LINE__ NT.eq nt1 nt2 in
    let typeself, prop1, prop2 =
      if String.equal name1 name2 then (name1, prop1, prop2)
      else (name1, P.peval prop1, P.subst_id prop2 name2 name1)
    in
    let ot_uqvs, ot_pre = solve pres in
    let prop1 = P.peval prop1 in
    let prop2 = P.peval prop2 in
    let nu = { ty = nt; x = typeself } in
    let () =
      Typectx.pretty_print_q
        (List.map (fun x -> x.x) ot_uqvs @ [ nu.x ])
        []
        (And [ ot_pre; prop2 ])
        prop1
    in
    let eq2, pre2 = P.assume_tope_uprop __FILE__ __LINE__ prop2 in
    let final_eqvs, final_post =
      P.assume_tope_uprop_fresh_name __FILE__ __LINE__ prop1
    in
    let final_pre = P.And [ ot_pre; pre2 ] in
    let final_uqvs = ot_uqvs @ (nu :: eq2) in
    do_check file line (final_uqvs, final_eqvs, final_pre, final_post)
  in
  let rec aux pres ctx (t1, t2) =
    match Typectx.destrct_right ctx with
    | None -> solve_pres pres (t1, t2)
    | Some (ctx, (id, MMT.Ot oty)) -> aux (pres @ [ (id, oty) ]) ctx (t1, t2)
    | Some (ctx, (id, MMT.Ut uty)) ->
        let t1 = Typectx.close_by_diff_ [ (id, uty) ] t1 in
        let t2 = Typectx.close_by_diff_ [ (id, uty) ] t2 in
        aux pres ctx (t1, t2)
  in
  aux [] ctx (t1, t2)

let subtyping_check file line (ctx : Typectx.ctx) (inferred_ty : UT.t)
    (target_ty : UT.t) =
  let open UT in
  (* let () = if !counter == 1 then failwith "end" else counter := !counter + 1 in *)
  let () =
    Typectx.pretty_print_subtyping ctx (MMT.Ut inferred_ty, MMT.Ut target_ty)
  in
  let rec aux ctx (t1, t2) =
    match (t1, t2) with
    | UnderTy_base _, UnderTy_base _ -> check_under_ctx file line ctx (t1, t2)
    | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
        List.iter (check_under_ctx file line ctx)
        @@ _safe_combine __FILE__ __LINE__ ts1 ts2
    | ( UnderTy_under_arrow { argty = t11; retty = t12 },
        UnderTy_under_arrow { argty = t21; retty = t22 } ) ->
        aux ctx (t21, t11);
        aux ctx (t12, t22)
    | ( UnderTy_over_arrow { argname = x1; argty = t11; retty = t12 },
        UnderTy_over_arrow { argname = x2; argty = t21; retty = t22 } ) ->
        let () = aux ctx (ot_to_ut t11, ot_to_ut t21) in
        let x' = Rename.unique x2 in
        let t12 = subst_id t12 x1 x' in
        let t22 = subst_id t22 x2 x' in
        let ctx = Typectx.ot_add_to_right ctx (x', t21) in
        aux ctx (t12, t22)
    | _, _ -> _failatwith __FILE__ __LINE__ "die: under subtype"
  in
  aux ctx (inferred_ty, target_ty)

let subtyping_check_bool file line (ctx : Typectx.ctx) (inferred_ty : UT.t)
    (target_ty : UT.t) =
  try
    let _ = subtyping_check file line ctx inferred_ty target_ty in
    true
  with
  | Autov.FailWithModel (msg, _) ->
      let () = Pp.printf "@{<orange>Type Check failed:@}%s\n" msg in
      false
  | Autov.SMTTIMEOUT ->
      let () = Pp.printf "@{<orange>Type Check failed:@}%s\n" "timeout" in
      false
  | e -> raise e
