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

let context_convert (ctx : UT.t Typectx.t) (name, nt, prop1, prop2) =
  let nu = (NT.to_smtty nt, name) in
  let open Autov.Prop in
  let ctx = List.rev ctx in
  let rec aux ctx (prop1, prop2) =
    match ctx with
    | [] -> mk_forall nu (fun _ -> Implies (prop2, prop1))
    | (x, xty) :: ctx -> (
        (* let () = *)
        (*   Printf.printf "x: %s P1: %s -> (%s)  P2: %s ~> (%s)\n" x *)
        (*     (Autov.layout_prop prop1) *)
        (*     (Zzdatatype.Datatype.StrList.to_string @@ Autov.prop_fv prop1) *)
        (*     (Autov.layout_prop prop2) *)
        (*     (Zzdatatype.Datatype.StrList.to_string @@ Autov.prop_fv prop2) *)
        (* in *)
        match
          ( List.exists (String.equal x) @@ Autov.prop_fv prop1,
            List.exists (String.equal x) @@ Autov.prop_fv prop2 )
        with
        | false, false -> aux ctx (prop1, prop2)
        | true, false ->
            let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in
            aux ctx (mk_exists x (fun _ -> And [ xprop; prop1 ]), prop2)
        | false, true ->
            let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in
            aux ctx (prop1, mk_forall x (fun _ -> Implies (xprop, prop2)))
        | true, true ->
            let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in
            mk_forall x (fun _ -> Implies (xprop, aux ctx (prop1, prop2))))
  in
  let q = aux ctx (prop1, prop2) in
  (* closing check *)
  match Autov.prop_fv q with
  | [] -> q
  | fv ->
      _failatwith __FILE__ __LINE__
        (spf "FV: %s" @@ Zzdatatype.Datatype.StrList.to_string fv)

let subtyping_check file line (ctx : UT.t Typectx.t) (t1 : UT.t) (t2 : UT.t) =
  let open UT in
  let rec aux ctx (t1, t2) =
    let () = Printf.printf "Subtype: %s\n" @@ layout_subtyping ctx (t1, t2) in
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
        let q = context_convert ctx (typeself, nt, prop1, prop2) in
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
