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

let close_qv_by_diff ctx ctx' ({ uqvs; eqvs; qbody } : Quantified.Qunderty.t) :
    Quantified.Qunderty.t =
  let uqvs', eqvs' = subtract ctx ctx' in
  let ctx'' =
    List.map (fun (id, tys) -> (id, Underty.T.conjunct_list tys))
    @@ Typectx.UnderTypectx.subtract ctx.qbody ctx'.qbody
  in
  let eqvs'', props =
    List.split
    @@ List.filter_map
         (fun (x, ty) ->
           match Underty.T.assume_base_destruct_opt ty with
           | None -> None
           | Some (x', ty, prop) ->
               Some ({ ty; x }, Autov.Prop.subst_id prop x' x))
         ctx''
  in
  {
    uqvs = uqvs @ uqvs';
    eqvs = eqvs @ eqvs' @ eqvs'';
    qbody = Underty.T.map_on_retty (fun q -> And (props @ [ q ])) qbody;
  }

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
