module NL = Languages.NormalAnormal
module UL = Languages.UnderAnormal
module NT = Languages.Normalty
module UT = Languages.Underty
module Op = Languages.Op
module P = Autov.Prop
open Zzdatatype.Datatype
open Sugar

let layout_subtyping = Frontend.Typectx.pretty_layout_under_subtyping

let _assume_basety file line (x, ty) =
  let open UT in
  match ty with
  | UnderTy_base { basename; prop; normalty } ->
      let prop = P.subst_id prop basename x in
      ((NT.to_smtty normalty, x), prop)
  | _ -> _failatwith file line "should not happen"

let context_convert (ctx : UT.bodyt Typectx.t) uqvs (name, nt, prop1, prop2) =
  let top_uq prop =
    List.fold_right
      UT.(
        fun { ty; x } prop ->
          Autov.Prop.mk_forall (NT.to_smtty ty, x) (fun _ -> prop))
      uqvs prop
  in
  let nu = (NT.to_smtty nt, name) in
  let open Autov.Prop in
  let aux (x, xty) (outter, prop1, prop2) =
    match
      ( List.exists (String.equal x) @@ Autov.prop_fv prop1,
        List.exists (String.equal x) @@ Autov.prop_fv prop2 )
    with
    | false, false -> (outter, prop1, prop2)
    | true, false ->
        let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in
        (outter, mk_exists x (fun _ -> And [ xprop; prop1 ]), prop2)
    | false, true ->
        let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in
        (outter, prop1, mk_forall x (fun _ -> Implies (xprop, prop2)))
    | true, true ->
        let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in
        ( (fun prop -> outter (mk_forall x (fun _ -> Implies (xprop, prop)))),
          prop1,
          prop2 )
  in
  let top_uq, prop1, prop2 =
    Typectx.fold_right aux ctx (top_uq, prop1, prop2)
  in
  let q = top_uq @@ mk_forall nu (fun _ -> Implies (prop2, prop1)) in
  (* let _ = *)
  (*   Printf.printf "uqvs: %s\n" *)
  (*     (Zzdatatype.Datatype.List.split_by_comma (fun x -> x.UT.x) uqvs) *)
  (* in *)
  (* closing check *)
  match Autov.prop_fv q with
  | [] -> q
  | fv ->
      _failatwith __FILE__ __LINE__
        (spf "FV: %s" @@ Zzdatatype.Datatype.StrList.to_string fv)

let subtyping_check file line (ctx : UT.bodyt Typectx.t UT.qted) (t1 : UT.t)
    (t2 : UT.t) =
  let open UT in
  let t1, ctx = unify_qv_to t1 ctx in
  let t2, { uqvs; eqvs; k = ctx } = unify_qv_to t2 ctx in
  let ctx =
    List.fold_right
      (fun qv ctx -> Typectx.add_to_left (eqv_to_bodyt qv, qv.x) ctx)
      eqvs ctx
  in
  let rec aux ctx (t1, t2) =
    let () =
      Printf.printf "Subtype: ∀(%s) %s\n"
        (Zzdatatype.Datatype.List.split_by_comma (fun x -> x.UT.x) uqvs)
      @@ layout_subtyping ctx (t1, t2)
    in
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
        let q = context_convert ctx uqvs (typeself, nt, prop1, prop2) in
        let () = Printf.printf "VC: %s\n" @@ Autov.coq_layout_prop q in
        if Autov.check q then ()
        else _failatwith file line "Subtyping check: rejected by the verifier"
    | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
        List.iter (aux ctx) @@ List.combine ts1 ts2
    | ( UnderTy_arrow { argname = x1; argty = t11; retty = t12 },
        UnderTy_arrow { argname = x2; argty = t21; retty = t22 } ) ->
        let t22 = subst_id t22 x2 x1 in
        let () = aux ctx (t21, t11) in
        let () = aux ctx (t12, t22) in
        ()
    | _, _ -> _failatwith __FILE__ __LINE__ "die: under subtype"
  in
  aux ctx (t1, t2)
