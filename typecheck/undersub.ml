open Languages
module P = Autov.Prop
module Typectx = UnderTypectx
open Zzdatatype.Datatype
open Sugar
open Ntyped
open Abstraction

let with_lemma_to_query lemmas x =
  let pre, a, b = Lemma.query_with_lemma_to_prop @@ Lemma.with_lemma lemmas x in
  (* let () = Lemma.print_with_lemma (pre, b) in *)
  (pre, a, b)

let _assume_basety file line (x, ty) =
  let open UT in
  match ty with
  | UnderTy_base { basename; prop; normalty } ->
      let prop = P.subst_id prop basename x in
      ({ ty = normalty; x }, prop)
  | _ ->
      let () =
        Printf.printf " %s: %s\n" x (Frontend.Underty.pretty_layout ty)
      in
      _failatwith file line "should not happen"

let typed_to_smttyped = Languages.Ntyped.to_smttyped

let to_query (nu, prop1, prop2) =
  (* let () = *)
  (*   Typectx.pretty_print_q *)
  (*     (List.map (fun x -> x.x) [ nu ]) *)
  (*     (List.map (fun x -> x.x) []) *)
  (*     prop2 prop1 *)
  (* in *)
  let eq2, final_pre = P.assume_tope_uprop __FILE__ __LINE__ prop2 in
  (* let _ = *)
  (*   Pp.printf "@{<bold>LIFT:@}\n%s --->\n%s\n" *)
  (*     (Autov.pretty_layout_prop uprop2) *)
  (*     (Autov.pretty_layout_prop @@ P.topu_to_prop *)
  (*     @@ P.lift_uprop __FILE__ __LINE__ uprop2) *)
  (* in *)
  (* let uq2, final_pre = P.lift_uprop __FILE__ __LINE__ uprop2 in *)
  (* let uq2, final_pre = P.lift_merge_uprop __FILE__ __LINE__ uprop2 in *)
  let final_eqvs, final_post =
    P.assume_tope_uprop_fresh_name __FILE__ __LINE__ prop1
  in
  (* let _ = *)
  (*   Pp.printf "@{<bold>LIFT:@}\n%s --->\n%s\n" *)
  (*     (Autov.pretty_layout_prop prop1) *)
  (*     (Autov.pretty_layout_prop final_post) *)
  (* in *)
  (* let () = failwith "zz" in *)
  let final_uqvs = nu :: eq2 in
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
  | [] -> (pres, q)
  | fv ->
      let () = Printf.printf "q: %s\n" @@ Autov.pretty_layout_prop q in
      let () = Printf.printf "prop1: %s\n" @@ Autov.pretty_layout_prop prop1 in
      let () = Printf.printf "prop2: %s\n" @@ Autov.pretty_layout_prop prop2 in
      _failatwith __FILE__ __LINE__
        (spf "FV: %s" @@ Zzdatatype.Datatype.StrList.to_string fv)

let check file line pres q =
  match Autov.check pres q with
  | None -> ()
  | Some m ->
      (* let _ = Autov.Func_interp.get_preds_interp m in *)
      Autov._failwithmodel file line "Subtyping check: rejected by the verifier"
        m

(* let counter = ref 0 *)

let subtyping_check file line (ctx : Typectx.t) (inferred_ty : UT.t)
    (target_ty : UT.t) =
  let open UT in
  (* let () = if !counter == 1 then failwith "end" else counter := !counter + 1 in *)
  let () = Typectx.pretty_print_subtyping ctx (inferred_ty, target_ty) in
  let rec aux ctx1 ctx2 (t1, t2) =
    match (t1, t2) with
    | ( UnderTy_base { basename = name1; prop = prop1; normalty = nt1 },
        UnderTy_base { basename = name2; prop = prop2; normalty = nt2 } ) ->
        let nt = _check_equality __FILE__ __LINE__ NT.eq nt1 nt2 in
        let typeself, prop1, prop2 =
          if String.equal name1 name2 then (name1, prop1, prop2)
          else (name1, prop1, P.subst_id prop2 name2 name1)
        in
        let nu = { ty = nt; x = typeself } in
        let prop1' = Typectx.close_prop_drop_independt ctx1 prop1 in
        (* let _ = *)
        (*   Pp.printf "@{<bold>LIFT:@}\n%s --->\n%s\n" *)
        (*     (Autov.pretty_layout_prop prop1') *)
        (*     (Autov.pretty_layout_prop @@ P.topu_to_prop *)
        (*     @@ P.lift_uprop __FILE__ __LINE__ prop1') *)
        (* in *)
        (* let () = failwith "zz" in *)
        let prop2' = Typectx.close_prop_drop_independt ctx2 prop2 in
        let () = Typectx.pretty_print_q [ nu.x ] [] prop2' prop1' in
        (* let () = failwith "zz" in *)
        let pres, q = to_query (nu, prop1', prop2') in
        (* let () = *)
        (*   if !counter == 1 then failwith "zz" else counter := !counter + 1 *)
        (* in *)
        (* let _ = failwith "end" in *)
        (* let pres, q = context_convert ctx (nu, prop1, prop2) in *)
        check file line pres q
    | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
        let rec loop ctx1 ctx2 = function
          | [], [] -> ()
          | (_, ty1) :: ts1, (name2, ty2) :: ts2 ->
              let () = aux ctx1 ctx2 (ty1, ty2) in
              (* let ctx1 = Typectx.add_to_right ctx1 { x = name1; ty = ty1 } in *)
              let ctx2 =
                Typectx.force_add_to_right ctx2 { x = name2; ty = ty2 }
              in

              let () = Typectx.pretty_print ctx2 in
              loop ctx1 ctx2 (ts1, ts2)
          | _, _ -> _failatwith __FILE__ __LINE__ ""
        in
        loop ctx1 ctx2 (ts1, ts2)
    | ( UnderTy_arrow { argname = x1; argty = t11; retty = t12 },
        UnderTy_arrow { argname = x2; argty = t21; retty = t22 } ) ->
        let () = aux ctx1 ctx2 (t21, t11) in
        let ctx1 = Typectx.force_add_to_right ctx1 { x = x1; ty = t11 } in
        let ctx2 = Typectx.force_add_to_right ctx2 { x = x2; ty = t21 } in
        let () = aux ctx1 ctx2 (t12, t22) in
        ()
    | _, _ -> _failatwith __FILE__ __LINE__ "die: under subtype"
  in
  aux ctx ctx (inferred_ty, target_ty)

let subtyping_check_bool file line (ctx : Typectx.t) (inferred_ty : UT.t)
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
