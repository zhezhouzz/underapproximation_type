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

type mode = InIn | InNotin | NotinIn | NotinNotin

let core ctx nu ((eqvs1, uprop1), prop2) =
  let open P in
  let check_in x p = List.exists (String.equal x) @@ Autov.prop_fv p in
  let rec aux ctx ((eqvs1, uprop1), (eqvs2, uprop2)) =
    match Typectx.destrct_right ctx with
    | None ->
        let _ =
          Printf.printf "%s, %s\n"
            (List.split_by_comma (fun x -> x.x) eqvs2)
            (Autov.pretty_layout_prop uprop2)
        in
        let eqvs1' = List.map (fun x -> Ntyped.map Rename.unique x) eqvs1 in
        let uprop1' = P.rename_prop uprop1 (List.combine eqvs1 eqvs1') in
        let uprop1' = P.peval uprop1' in
        let uprop2 = P.peval uprop2 in
        (* let _ = failwith "end" in *)
        (nu :: eqvs2, eqvs1', uprop2, uprop1')
    | Some (ctx, (x, xty)) ->
        let xty = UT.conjunct_list xty in
        let eqvs1, uprop1 =
          let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in
          conjunct_base_to_tope_uprop_ ([ x ], xprop) (eqvs1, uprop1)
        in
        let eqvs2, uprop2 =
          if check_in x uprop2 then
            let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in
            conjunct_base_to_tope_uprop_ ([ x ], xprop) (eqvs2, uprop2)
          else (eqvs2, uprop2)
        in
        (* let update (eqvs, uprop) = *)
        (*   if check_in x uprop then *)
        (*     let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in *)
        (*     conjunct_base_to_tope_uprop_ ([ x ], xprop) (eqvs, uprop) *)
        (*   else (eqvs, uprop) *)
        (* in *)
        (* aux ctx (update (eqvs1, uprop1), update (eqvs2, uprop2)) *)
        aux ctx ((eqvs1, uprop1), (eqvs2, uprop2))
    (* let in1, in2 = (check_in x uprop1, check_in x uprop2) in *)
    (* match (in1, in2) with *)
    (* | false, false -> aux ctx ((eqvs1, uprop1), (eqvs2, uprop2)) *)
    (* | true, false -> *)
    (*     let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in *)
    (*     let eqvs1, prop1 = *)
    (*       conjunct_base_to_tope_uprop_ ([ x ], xprop) (eqvs1, uprop1) *)
    (*     in *)
    (*     aux ctx ((eqvs1, prop1), (eqvs2, uprop2)) *)
    (* | _, true -> *)
    (*     let x, _ = _assume_basety __FILE__ __LINE__ (x, xty) in *)
    (*     aux ctx ((eqvs1 [ x ], uprop1), (eqvs2 @ [ x ], uprop2)) *)
    (* let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in *)
    (* aux ctx ((x :: uqvs, xprop :: upre), (eqvs1, uprop1), (uqvs2, prop2)) *)
  in
  let eqvs2, uprop2 = assume_tope_uprop __FILE__ __LINE__ prop2 in
  let _ = Printf.printf "%s\n" (Autov.pretty_layout_prop prop2) in
  let _ = Printf.printf "%s\n" (Autov.pretty_layout_prop uprop2) in
  aux ctx ((eqvs1, uprop1), (eqvs2, uprop2))

let context_convert (ctx : Typectx.t) (nu, prop1, prop2) =
  (* let () = Pp.printf "%s\n" (Autov.pretty_layout_prop prop1) in *)
  let eq1, prop1 = P.assume_tope_uprop __FILE__ __LINE__ prop1 in
  (* let uq2, prop2 = P.assume_tope_uprop __FILE__ __LINE__ prop2 in *)
  let final_uqvs, final_eqvs, final_pre, final_post =
    core ctx nu ((eq1, prop1), prop2)
  in
  let () =
    Typectx.pretty_print_q
      (List.map (fun x -> x.x) final_uqvs)
      (List.map (fun x -> x.x) final_eqvs)
      final_pre final_post
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

let subtyping_check file line (ctx : Typectx.t) (inferred_ty : UT.t)
    (target_ty : UT.t) =
  let open UT in
  let () = Typectx.pretty_print_subtyping ctx (inferred_ty, target_ty) in
  let rec aux ctx (t1, t2) =
    match (t1, t2) with
    | ( UnderTy_base { basename = name1; prop = prop1; normalty = nt1 },
        UnderTy_base { basename = name2; prop = prop2; normalty = nt2 } ) ->
        let nt = _check_equality __FILE__ __LINE__ NT.eq nt1 nt2 in
        let typeself, prop1, prop2 =
          if String.equal name1 name2 then (name1, prop1, prop2)
          else (name1, prop1, P.subst_id prop2 name2 name1)
        in
        let nu = { ty = nt; x = typeself } in
        let prop1' = Typectx.close_prop ctx prop1 in
        (* let _ = *)
        (*   Pp.printf "@{<bold>LIFT:@}\n%s --->\n%s\n" *)
        (*     (Autov.pretty_layout_prop prop1') *)
        (*     (Autov.pretty_layout_prop @@ P.topu_to_prop *)
        (*     @@ P.lift_uprop __FILE__ __LINE__ prop1') *)
        (* in *)
        (* let () = failwith "zz" in *)
        let prop2' = Typectx.close_prop_drop_independt ctx prop2 in
        let pres, q = to_query (nu, prop1', prop2') in
        (* let _ = failwith "end" in *)
        (* let pres, q = context_convert ctx (nu, prop1, prop2) in *)
        check file line pres q
    | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
        List.iter (aux ctx) @@ List.combine ts1 ts2
    | ( UnderTy_arrow { argname = x1; argty = t11; retty = t12 },
        UnderTy_arrow { argname = x2; argty = t21; retty = t22 } ) ->
        let t22 = subst_id t22 x2 x1 in
        (* let () = *)
        (*   if UT.is_fv_in x2 t22 then aux ctx (t11, t21) else aux ctx (t21, t11) *)
        (* in *)
        let () = aux ctx (t21, t11) in
        let () = aux ctx (t12, t22) in
        ()
    | _, _ -> _failatwith __FILE__ __LINE__ "die: under subtype"
  in
  aux ctx (inferred_ty, target_ty)
