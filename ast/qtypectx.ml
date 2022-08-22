include Quantified.F (Normalty.T) (Typectx.UnderTypectx)

(* open Sugar *)
(* open Underty.T *)
module NT = Normalty.T
open Typed.F (Normalty.T)

let unify_raw (t1 : Quantified.Qunderty.t) (t2 : t) =
  let qbody, t2 = unify (t1.uqvs, t1.eqvs, t1.qbody, Underty.T.subst_id) t2 in
  (qbody, t2)

let unify (t1 : Quantified.Qunderty.t) (t2 : t) : Quantified.Qunderty.t * t =
  let qbody, t2 = unify_raw t1 t2 in
  (({ uqvs = t2.uqvs; eqvs = t2.eqvs; qbody } : Quantified.Qunderty.t), t2)

let unifys ts target =
  List.fold_right
    (fun t (ts, target) ->
      let t', target = unify t target in
      (t' :: ts, target))
    ts ([], target)

let add_to_right ctx (ty, x) =
  { ctx with qbody = Typectx.UnderTypectx.add_to_right ctx.qbody (ty, x) }

let add_to_rights ctx l = List.fold_left add_to_right ctx l

let simp_retty_v1 eqvs props retty =
  (eqvs, Underty.T.map_on_retty (fun q -> And (props @ [ q ])) retty)

let simp_retty_v2 eqvs props retty =
  let final_eqvs = ref [] in
  let mk q =
    List.fold_left
      (fun (eqvs, qbody) qv ->
        (* let () = *)
        (*   Printf.printf "WORK ON %s in %s\n" qv.x *)
        (*     (Autov.pretty_layout_prop qbody) *)
        (* in *)
        match Autov.Prop.simp_exists qv.x qbody with
        | false, qbody ->
            (* let () = *)
            (*   Printf.printf "Drop %s and get %s\n" qv.x *)
            (*     (Autov.pretty_layout_prop qbody) *)
            (* in *)
            (eqvs, qbody)
        | true, qbody -> (eqvs @ [ qv ], qbody))
      ([], And (props @ [ q ]))
      eqvs
  in
  let retty =
    Underty.T.map_on_retty
      (fun q ->
        let eqvs, q = mk q in
        let () = final_eqvs := eqvs in
        q)
      retty
  in
  (!final_eqvs, retty)

let close_qv_by_diff ctx ctx' ({ uqvs; eqvs; qbody } : Quantified.Qunderty.t) :
    Quantified.Qunderty.t =
  let uqvs', eqvs' = subtract ctx ctx' in
  let ctx'' =
    List.map (fun (ifq, (id, tys)) ->
        let ty = Underty.T.conjunct_list tys in
        (ifq, (id, ty)))
    @@ Typectx.UnderTypectx.subtract ctx.qbody ctx'.qbody
  in
  let eqvs'', props =
    List.split
    @@ List.filter_map
         (fun (ifq, (x, ty)) ->
           match Underty.T.assume_base_destruct_opt ty with
           | None -> None
           | Some (x', ty, prop) ->
               let prop = Autov.Prop.subst_id prop x' x in
               Some ((if ifq then [ { ty; x } ] else []), prop))
         ctx''
  in
  let eqvs'' = List.concat eqvs'' in
  let eqvs = eqvs @ eqvs' @ eqvs'' in
  let eqvs, qbody = simp_retty_v2 eqvs props qbody in
  { uqvs = uqvs @ uqvs'; eqvs; qbody }

(* let hide_vars_in_ctx ctx vars ty = *)
(*   List.fold_right *)
(*     (fun id { uqvs; eqvs; k } -> *)
(*       let idty = *)
(*         match Typectx.get_opt ctx.k id with *)
(*         | None -> _failatwith __FILE__ __LINE__ "" *)
(*         | Some idty -> idty *)
(*       in *)
(*       { uqvs; eqvs; k = hide_quantify_variable_in_bodyt id idty k }) *)
(*     vars ty *)

let empty = without_qv Typectx.UnderTypectx.empty

(* let fv f { uqvs; eqvs; k } = *)
(*   List.map (fun x -> x.x) uqvs @ List.map (fun x -> x.x) eqvs @ Typectx.fv f k *)
