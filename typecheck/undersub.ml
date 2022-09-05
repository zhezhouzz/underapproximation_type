module NL = Languages.NormalAnormal
module UL = Languages.UnderAnormal
module NT = Languages.Normalty
module UT = Languages.Underty
module QUT = Languages.Qunderty
module Op = Languages.Op
module P = Autov.Prop
module SMTtyped = Languages.SMTtyped
module Typectx = Languages.UnderTypectx
module Qtypectx = Languages.Qtypectx
open Zzdatatype.Datatype
open Sugar
open Languages.Ntyped
open Abstraction

(* let layout_subtyping = Frontend.Typectx.pretty_layout_under_subtyping *)

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

let core ctx nu ((eq1, prop1), (uq2, prop2)) =
  let open P in
  let check_in x p = List.exists (String.equal x) @@ Autov.prop_fv p in
  let rec aux ctx ((uqvs, pre), (eq1, prop1), (uq2, prop2)) =
    match Typectx.destrct_right ctx with
    | None -> ((uqvs, pre), eq1 @ uq2, Implies (prop2, prop1))
    | Some (ctx, (x, xty)) -> (
        let xty = UT.conjunct_list xty in
        let in1, in2 = (check_in x prop1, check_in x prop2) in
        (* let () = *)
        (*   Printf.printf "WORK ON... %s: %s <%b;%b> |- (%s ==> ∃ %s, %s) \n" x *)
        (*     (Frontend.Underty.pretty_layout xty) *)
        (*     in2 in1 *)
        (*     (Autov.pretty_layout_prop prop2) *)
        (*     (List.split_by_comma (fun x -> x.P.x) eq1) *)
        (*     (Autov.pretty_layout_prop prop1) *)
        (* in *)
        match (in1, in2) with
        | false, false -> aux ctx ((uqvs, pre), (eq1, prop1), (uq2, prop2))
        | true, false ->
            let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in
            let x = typed_to_smttyped x in
            let if_keep, prop1 = add_with_simp_eq_prop x xprop prop1 in
            let eq1 = if if_keep then eq1 @ [ x ] else eq1 in
            aux ctx ((uqvs, pre), (eq1, prop1), (uq2, prop2))
        | _, true ->
            let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in
            let x = typed_to_smttyped x in
            aux ctx ((x :: uqvs, xprop :: pre), (eq1, prop1), (uq2, prop2)))
  in
  aux ctx (([ nu ], []), (eq1, prop1), (uq2, prop2))

let context_convert (uqvs : string SMTtyped.typed list) (ctx : Typectx.t)
    (name, nt, prop1, eqvs2, prop2) =
  let nu = typed_to_smttyped { ty = nt; x = name } in
  let open SMTtyped in
  let mk_q (uqs, eqs, prop) =
    let _, basic_uqs = List.partition (fun x -> is_dt x.ty) uqs in
    let prop =
      Lemma.with_lemma (Prim.lemmas_to_pres ()) prop (basic_uqs @ eqs)
    in
    let dt_eqs, basic_eqs =
      List.partition (fun x -> Autov.Smtty.is_dt x.ty) eqs
    in
    let dt_eqs, prop =
      Autov.uqv_encoding (List.map (fun x -> x.x) dt_eqs) prop
    in
    let prop =
      List.fold_right (fun qv prop -> P.(Exists (qv, prop))) dt_eqs prop
    in
    List.fold_right (fun x prop -> P.Forall (x, prop)) uqs
    @@ List.fold_right (fun x prop -> P.Exists (x, prop)) basic_eqs prop
  in
  (* let check_in x p = List.exists (String.equal x) @@ Autov.prop_fv p in *)
  (* let aux (x, xty) ((eqpre, pre), (eq1, prop1), prop2) = *)
  (*   let xty = UT.conjunct_list xty in *)
  (*   let in1, in2 = (check_in x prop1, check_in x prop2) in *)
  (*   let () = *)
  (*     Printf.printf *)
  (*       "WORK ON... %s: %s <%b;%b> |- ∃ %s, %s /\\ (%s ==> ∃ %s, %s) \n" x *)
  (*       (Frontend.Underty.pretty_layout xty) *)
  (*       in2 in1 *)
  (*       (List.split_by_comma (fun x -> x.P.x) eqpre) *)
  (*       (Autov.pretty_layout_prop pre) *)
  (*       (Autov.pretty_layout_prop prop2) *)
  (*       (List.split_by_comma (fun x -> x.P.x) eq1) *)
  (*       (Autov.pretty_layout_prop prop1) *)
  (*   in *)
  (*   match (in1, in2) with *)
  (*   | false, false -> ((eqpre, pre), (eq1, prop1), prop2) *)
  (*   | true, false -> *)
  (*       let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in *)
  (*       let x = typed_to_smttyped x in *)
  (*       let if_keep, prop1 = add_with_simp_eq_prop x xprop prop1 in *)
  (*       let eq1 = if if_keep then eq1 @ [ x ] else eq1 in *)
  (*       ((eqpre, pre), (eq1, prop1), prop2) *)
  (*   | _, true -> *)
  (*       let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in *)
  (*       let x = typed_to_smttyped x in *)
  (*       ((eqpre @ [ x ], P.And [ pre; xprop ]), (eq1, prop1), prop2) *)
  (* in *)
  let eq1, prop1 = P.lift_exists prop1 in
  let uq2, prop2 =
    let eq2, prop2 = P.lift_exists prop2 in
    let () =
      if List.length eq2 != 0 then _failatwith __FILE__ __LINE__ "" else ()
    in
    List.fold_right
      (fun fv (eqvs, prop) ->
        if Typectx.exists ctx fv then (eqvs, prop)
        else
          match List.find_opt (fun x -> String.equal x.x fv) uqvs with
          | None -> failwith (spf "filter_qvs_by_find: %s" fv)
          | Some x ->
              let x' = { x = Rename.unique x.x; ty = x.ty } in
              (eqvs @ [ x' ], P.subst_id prop x.x x'.x))
      (List.substract String.equal (Autov.prop_fv prop2) [ nu.x ])
      ([], prop2)
  in
  (* let eq2, prop2 = *)
  (*   List.fold_left *)
  (*     (fun (eq2, prop2) u -> *)
  (*       if List.exists (fun x -> String.equal u.x x.x) (eq2 @ eq1) then *)
  (*         let u' = { ty = u.ty; x = Rename.unique_ u.x } in *)
  (*         (eq2 @ [ u' ], P.subst_id prop2 u.x u'.x) *)
  (*       else (eq2 @ [ u ], prop2)) *)
  (*     ([], prop2) eq2 *)
  (* in *)
  let (uqvs', pre), eqvs', prop = core ctx nu ((eq1, prop1), (uq2, prop2)) in
  (* let (eqpre, pre), (eq1, prop1), prop2 = *)
  (*   Typectx.fold_right aux ctx (([], P.mk_true), (eq1, prop1), prop2) *)
  (* in *)
  (* let pre, prop1, prop2 = P.(map3 simp (And pre, And prop1, And prop2)) in *)
  let final_uqvs = uqvs @ uqvs' in
  let final_eqvs = eqvs2 @ eqvs' in
  let final_prop =
    match pre with [] -> prop | pre -> Implies (And pre, prop)
  in
  let () =
    Frontend.Qtypectx.pretty_print_q
      (List.map (fun x -> x.x) final_uqvs)
      (List.map (fun x -> x.x) final_eqvs)
      final_prop
  in
  let q = mk_q (final_uqvs, final_eqvs, final_prop) in
  (* let q = P.simp_conj_disj q in *)
  (* let () = Printf.printf "q: %s\n" @@ Autov.pretty_layout_prop q in *)
  (* let () = Printf.printf "prop1: %s\n" @@ Autov.pretty_layout_prop prop1 in *)
  (* let () = Printf.printf "prop2: %s\n" @@ Autov.pretty_layout_prop prop2 in *)
  (* closing check *)
  match Autov.prop_fv q with
  | [] -> q
  | fv ->
      let () = Printf.printf "q: %s\n" @@ Autov.pretty_layout_prop q in
      let () = Printf.printf "prop1: %s\n" @@ Autov.pretty_layout_prop prop1 in
      let () = Printf.printf "prop2: %s\n" @@ Autov.pretty_layout_prop prop2 in
      _failatwith __FILE__ __LINE__
        (spf "FV: %s" @@ Zzdatatype.Datatype.StrList.to_string fv)

let subtyping_check_with_hidden_vars file line (qctx : Qtypectx.t) (t1 : UT.t)
    eqvs (t2 : UT.t) =
  let open UT in
  let () = Frontend.Qtypectx.pretty_print_subtyping qctx (t1, t2) in
  match qctx with
  | { qvs = uqvs; qbody = ctx } ->
      let rec aux ctx (t1, t2) =
        match (t1, t2) with
        | ( UnderTy_base { basename = name1; prop = prop1; normalty = nt1 },
            UnderTy_base { basename = name2; prop = prop2; normalty = nt2 } ) ->
            (* let typeself, prop1, prop2 = *)
            (*   match (Typectx.in_ctx ctx name1, Typectx.in_ctx ctx name2) with *)
            (*   | true, true -> *)
            (*       ( _check_equality __FILE__ __LINE__ String.equal name1 name2, *)
            (*         prop1, *)
            (*         prop2 ) *)
            (*   | false, true -> (name2, P.subst_id prop1 name1 name2, prop2) *)
            (*   | _, _ -> (name1, prop1, P.subst_id prop2 name2 name1) *)
            (* in *)
            let nt = _check_equality __FILE__ __LINE__ NT.eq nt1 nt2 in
            let typeself, prop1, prop2 =
              if String.equal name1 name2 then (name1, prop1, prop2)
              else (name1, prop1, P.subst_id prop2 name2 name1)
            in
            let q =
              context_convert
                (List.map typed_to_smttyped uqvs)
                ctx
                (typeself, nt, prop1, List.map typed_to_smttyped eqvs, prop2)
            in
            (* let () = Printf.printf "VC: %s\n" @@ Autov.coq_layout_prop q in *)
            (* let () = Printf.printf "VC: %s\n" @@ Autov.pretty_layout_prop q in *)
            if Autov.check (List.map Lemma.to_prop @@ Prim.lemmas_to_pres ()) q
            then ()
            else
              _failatwith file line "Subtyping check: rejected by the verifier"
        | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
            List.iter (aux ctx) @@ List.combine ts1 ts2
        | ( UnderTy_arrow
              { argname = x1; hidden_vars = hv1; argty = t11; retty = t12 },
            UnderTy_arrow
              { argname = x2; hidden_vars = hv2; argty = t21; retty = t22 } ) ->
            let t22 = subst_id t22 x2 x1 in
            let ctx', t11 = Typectx.add_hidden_vars_to_right ctx (hv1, t11) in
            let ctx', t21 = Typectx.add_hidden_vars_to_right ctx' (hv2, t21) in
            let () = aux ctx' (t21, t11) in
            let () = aux ctx (t12, t22) in
            ()
        | _, _ -> _failatwith __FILE__ __LINE__ "die: under subtype"
      in
      aux ctx (t1, t2)

let subtyping_check file line (qctx : Qtypectx.t) (t1 : UT.t) (t2 : UT.t) =
  let open UT in
  let () = Frontend.Qtypectx.pretty_print_subtyping qctx (t1, t2) in
  match qctx with
  | { qvs = uqvs; qbody = ctx } ->
      let rec aux ctx (t1, t2) =
        match (t1, t2) with
        | ( UnderTy_base { basename = name1; prop = prop1; normalty = nt1 },
            UnderTy_base { basename = name2; prop = prop2; normalty = nt2 } ) ->
            let nt = _check_equality __FILE__ __LINE__ NT.eq nt1 nt2 in
            let typeself, prop1, prop2 =
              if String.equal name1 name2 then (name1, prop1, prop2)
              else (name1, prop1, P.subst_id prop2 name2 name1)
            in
            let q =
              context_convert
                (List.map typed_to_smttyped uqvs)
                ctx
                (typeself, nt, prop1, [], prop2)
            in
            if Autov.check (List.map Lemma.to_prop @@ Prim.lemmas_to_pres ()) q
            then ()
            else
              _failatwith file line "Subtyping check: rejected by the verifier"
        | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
            List.iter (aux ctx) @@ List.combine ts1 ts2
        | ( UnderTy_arrow
              { argname = x1; hidden_vars = []; argty = t11; retty = t12 },
            UnderTy_arrow
              { argname = x2; hidden_vars = []; argty = t21; retty = t22 } ) ->
            let t22 = subst_id t22 x2 x1 in
            let () =
              if UT.is_fv_in x2 t22 then aux ctx (t11, t21)
              else aux ctx (t21, t11)
            in
            let () = aux ctx (t12, t22) in
            ()
        | _, _ -> _failatwith __FILE__ __LINE__ "die: under subtype"
      in
      aux ctx (t1, t2)
