module NL = Languages.NormalAnormal
module UL = Languages.UnderAnormal
module NT = Languages.Normalty
module UT = Languages.Underty
module QUT = Languages.Qunderty
module Op = Languages.Op
module P = Autov.Prop
module Typectx = Languages.UnderTypectx
module Qtypectx = Languages.Qtypectx
open Zzdatatype.Datatype
open Sugar
open Languages.Ntyped

(* let layout_subtyping = Frontend.Typectx.pretty_layout_under_subtyping *)

let _assume_basety file line (x, ty) =
  let open UT in
  match ty with
  | UnderTy_base { basename; prop; normalty } ->
      let prop = P.subst_id prop basename x in
      ((NT.to_smtty normalty, x), prop)
  | _ -> _failatwith file line "should not happen"

type mode = InIn | InNotin | NotinIn | NotinNotin

let context_convert (ctx : Typectx.t)
    (uqvs : string Languages.Ntyped.typed list) (name, nt, prop1, prop2) =
  (* let top_uq prop = *)
  (*   List.fold_right *)
  (*     (fun { ty; x } prop -> *)
  (*       Autov.Prop.mk_forall (NT.to_smtty ty, x) (fun _ -> prop)) *)
  (*     uqvs prop *)
  (* in *)
  let to_qvs =
    List.map (fun Languages.Ntyped.{ ty; x } -> (NT.to_smtty ty, x))
  in
  let nu = (NT.to_smtty nt, name) in
  let open Autov.Prop in
  let mk_q (uqs, eqs, pre, prop1, prop2) =
    List.fold_right (fun x prop -> mk_forall x (fun _ -> prop)) uqs
    @@ List.fold_right (fun x prop -> mk_exists x (fun _ -> prop)) eqs
    @@ Implies (pre, Implies (prop2, prop1))
  in
  let check_in x p = List.exists (String.equal x) @@ Autov.prop_fv p in
  let aux (x, xty) (uqvs, eqvs, pre, prop1, prop2) =
    let xty = UT.conjunct_list xty in
    (* let () = *)
    (*   Printf.printf "WORK ON... %s: %s |- (%s ==> \n\t%s ==> \t%s) \n" x *)
    (*     (Frontend.Underty.pretty_layout xty) *)
    (*     (Autov.pretty_layout_prop pre) *)
    (*     (Autov.pretty_layout_prop prop2) *)
    (*     (Autov.pretty_layout_prop prop1) *)
    (* in *)
    let mode =
      if check_in x pre then InIn
      else
        match (check_in x prop1, check_in x prop2) with
        | true, true -> InIn
        | true, false -> InNotin
        | false, true -> NotinIn
        | false, false -> NotinNotin
    in
    match mode with
    | NotinNotin -> (uqvs, eqvs, pre, prop1, prop2)
    | InNotin ->
        let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in
        (uqvs, eqvs @ [ x ], pre, And [ xprop; prop1 ], prop2)
    | NotinIn ->
        let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in
        (uqvs, eqvs @ [ x ], pre, prop1, And [ xprop; prop2 ])
    | InIn ->
        let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in
        (uqvs @ [ x ], eqvs, And [ pre; xprop ], prop1, prop2)
  in
  let uqs, eqs, pre, prop1, prop2 =
    Typectx.fold_right aux ctx
      (to_qvs uqvs @ [ nu ], [], P.mk_true, prop1, prop2)
  in
  let () =
    Frontend.Qtypectx.pretty_print_q (List.map snd uqs) (List.map snd eqs) pre
      (prop1, prop2)
  in
  let q = mk_q (uqs, eqs, pre, prop1, prop2) in
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

let subtyping_check file line (qctx : Qtypectx.t) (t1 : QUT.t) (t2 : QUT.t) =
  let open UT in
  let () = Frontend.Qtypectx.pretty_print_subtyping qctx (t1, t2) in
  let t1, qctx' = Qtypectx.unify_raw t1 qctx in
  let t2, Qtypectx.{ uqvs; eqvs; qbody = ctx } = Qtypectx.unify_raw t2 qctx' in
  let ctx =
    List.fold_right
      (fun qv ctx -> Typectx.add_to_left (eqv_to_bodyt qv, qv.x) ctx)
      eqvs ctx
  in
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
        let q = context_convert ctx uqvs (typeself, nt, prop1, prop2) in
        (* let () = Printf.printf "VC: %s\n" @@ Autov.coq_layout_prop q in *)
        (* let () = Printf.printf "VC: %s\n" @@ Autov.pretty_layout_prop q in *)
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
