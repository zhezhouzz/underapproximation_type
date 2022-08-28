module ET = struct
  include Quantified.F (Normalty.T) (Typectx.UnderTypectx)
  open Sugar
  open Underty.T
  open Typed.Ntyped
  module UL = Anormal.UnderAnormal

  let add_hidden_vars_to_right { qvs; qbody } (hvs, ty) =
    let hvs, ty =
      List.fold_left
        (fun (hvs, ty) x ->
          let x' = { x = Rename.unique x.x; ty = x.ty } in
          (hvs @ [ x' ], subst_id ty x.x x'.x))
        ([], ty) hvs
    in
    ({ qvs = qvs @ hvs; qbody }, ty)

  let map f { qvs; qbody } = { qvs; qbody = f qbody }
  let conjunct q x = map (fun ctx -> Typectx.UnderTypectx.conjunct ctx x) q

  let add_to_right q x =
    map (fun ctx -> Typectx.UnderTypectx.add_to_right ctx x) q

  let add_to_rights q x =
    map (fun ctx -> Typectx.UnderTypectx.add_to_rights ctx x) q

  let add_arrow_arg_to_right q (hvs, x) =
    let q, ty = add_hidden_vars_to_right q (hvs, x.UL.ty) in
    add_to_right q UL.{ x = x.x; ty }

  let mk_from_qunder Qunder.{ qvs; qbody } =
    ({ qvs; qbody = Typectx.UnderTypectx.empty }, qbody)

  let close_by_diff { qvs; qbody } { qvs = qvs'; qbody = qbody' } uty =
    let rec subtract = function
      | a, [] -> a
      | h1 :: t1, h2 :: t2 ->
          if eq h1 h2 then subtract (t1, t2)
          else _failatwith __FILE__ __LINE__ ""
      | _, _ -> _failatwith __FILE__ __LINE__ ""
    in
    let eqvs = subtract (qvs, qvs') in
    let uty =
      List.fold_right
        (fun (ifq, (x, tys)) uty -> add_ex_prop ifq x (conjunct_list tys) uty)
        (Typectx.UnderTypectx.subtract qbody qbody')
        uty
    in
    List.fold_right add_ex_var eqvs uty

  let instantiate_qvs { qvs; qbody = ctx } Qunder.{ qvs = tyuqvs; qbody = uty }
      =
    let instantiate_var nt =
      Typectx.UnderTypectx.extract_vars_by_nt ctx nt
      @ List.filter_map
          (fun { ty; x } -> if NT.eq ty nt then Some x else None)
          qvs
    in
    let vars = List.map (fun x -> instantiate_var x.ty) tyuqvs in
    let open Zzdatatype.Datatype in
    let settings =
      List.map (fun l -> List.combine (List.map (fun x -> x.x) tyuqvs) l)
      @@ List.choose_list_list vars
    in
    instantiate_universial settings uty

  let fv { qvs; qbody } =
    List.filter (fun x -> not @@ List.exists (fun y -> String.equal x y.x) qvs)
    @@ Typectx.UnderTypectx.fv qbody

  open Zzdatatype.Datatype

  let var_space { qvs; qbody } =
    List.slow_rm_dup String.equal
    @@ List.map (fun x -> x.x) qvs
    @ Typectx.UnderTypectx.var_space qbody

  let eq a b = List.eq eq a.qvs b.qvs && Typectx.UnderTypectx.eq a.qbody b.qbody

  let subst_id { qvs; qbody } x z =
    if List.exists (fun y -> String.equal x y.x) qvs then { qvs; qbody }
    else { qvs; qbody = Typectx.UnderTypectx.subst_id qbody x z }

  let empty = { qvs = []; qbody = Typectx.UnderTypectx.empty }
  let get_ty { qbody; _ } x = Typectx.UnderTypectx.get_ty qbody x
end

include Quantified.F (Normalty.T) (ET)
open Sugar

(* open Underty.T *)
open Typed.Ntyped

let map f { qvs; qbody } = { qvs; qbody = f qbody }
let conjunct q x = map (fun ctx -> ET.conjunct ctx x) q
let get_ty { qbody; _ } x = ET.get_ty qbody x

let add_hidden_vars_to_right { qvs; qbody } x =
  let qbody, x = ET.add_hidden_vars_to_right qbody x in
  ({ qvs; qbody }, x)

let add_arrow_arg_to_right q x =
  map (fun ctx -> ET.add_arrow_arg_to_right ctx x) q

let add_to_right q x = map (fun ctx -> ET.add_to_right ctx x) q
let add_to_rights q x = map (fun ctx -> ET.add_to_rights ctx x) q
let mk_from_qunder Qunder.{ qvs; qbody } = ({ qvs; qbody = ET.empty }, qbody)

(* module NT = Normalty.T *)
(* open Typed.F (Normalty.T) *)

(* let unify_raw (t1 : Quantified.Qunderty.t) (t2 : t) = *)
(*   let qbody, t2 = unify (t1.uqvs, t1.eqvs, t1.qbody, Underty.T.subst_id) t2 in *)
(*   (qbody, t2) *)

(* let unify (t1 : Quantified.Qunderty.t) (t2 : t) : Quantified.Qunderty.t * t = *)
(*   let qbody, t2 = unify_raw t1 t2 in *)
(*   (({ uqvs = t2.uqvs; eqvs = t2.eqvs; qbody } : Quantified.Qunderty.t), t2) *)

(* let unifys ts target = *)
(*   List.fold_right *)
(*     (fun t (ts, target) -> *)
(*       let t', target = unify t target in *)
(*       (t' :: ts, target)) *)
(*     ts ([], target) *)

(* let add_to_right ctx (ty, x) = *)
(*   { ctx with qbody = Typectx.UnderTypectx.add_to_right ctx.qbody (ty, x) } *)

(* let add_to_rights ctx l = List.fold_left add_to_right ctx l *)

(* let simp_retty_v1 eqvs props retty = *)
(*   (eqvs, Underty.T.map_on_retty (fun q -> And (props @ [ q ])) retty) *)

(* let simp_retty_v2 eqvs props retty = *)
(*   let final_eqvs = ref [] in *)
(*   let mk q = *)
(*     List.fold_left *)
(*       (fun (eqvs, qbody) qv -> *)
(*         (\* let () = *\) *)
(*         (\*   Printf.printf "WORK ON %s in %s\n" qv.x *\) *)
(*         (\*     (Autov.pretty_layout_prop qbody) *\) *)
(*         (\* in *\) *)
(*         match Autov.Prop.simp_exists qv.x qbody with *)
(*         | false, qbody -> *)
(*             (\* let () = *\) *)
(*             (\*   Printf.printf "Drop %s and get %s\n" qv.x *\) *)
(*             (\*     (Autov.pretty_layout_prop qbody) *\) *)
(*             (\* in *\) *)
(*             (eqvs, qbody) *)
(*         | true, qbody -> (eqvs @ [ qv ], qbody)) *)
(*       ([], And (props @ [ q ])) *)
(*       eqvs *)
(*   in *)
(*   let retty = *)
(*     Underty.T.map_on_retty *)
(*       (fun q -> *)
(*         let eqvs, q = mk q in *)
(*         let () = final_eqvs := eqvs in *)
(*         q) *)
(*       retty *)
(*   in *)
(*   (!final_eqvs, retty) *)

let close_by_diff { qvs; qbody } { qvs = qvs'; qbody = qbody' } uty =
  if List.equal eq qvs qvs' then ET.close_by_diff qbody qbody' uty
  else _failatwith __FILE__ __LINE__ ""

let instantiate_qvs { qvs; qbody = ctx } ty =
  ET.instantiate_qvs { qvs = qvs @ ctx.qvs; qbody = ctx.qbody } ty
(* let close_qv_by_diff ctx ctx' ({ uqvs; eqvs; qbody } : Quantified.Qunderty.t) : *)
(*     Quantified.Qunderty.t = *)
(*   let uqvs', eqvs' = subtract ctx ctx' in *)
(*   let ctx'' = *)
(*     List.map (fun (ifq, (id, tys)) -> *)
(*         let ty = Underty.T.conjunct_list tys in *)
(*         (ifq, (id, ty))) *)
(*     @@ Typectx.UnderTypectx.subtract ctx.qbody ctx'.qbody *)
(*   in *)
(*   let eqvs'', props = *)
(*     List.split *)
(*     @@ List.filter_map *)
(*          (fun (ifq, (x, ty)) -> *)
(*            match Underty.T.assume_base_destruct_opt ty with *)
(*            | None -> None *)
(*            | Some (x', ty, prop) -> *)
(*                let prop = Autov.Prop.subst_id prop x' x in *)
(*                Some ((if ifq then [ { ty; x } ] else []), prop)) *)
(*          ctx'' *)
(*   in *)
(*   let eqvs'' = List.concat eqvs'' in *)
(*   let eqvs = eqvs @ eqvs' @ eqvs'' in *)
(*   let eqvs, qbody = simp_retty_v2 eqvs props qbody in *)
(*   { uqvs = uqvs @ uqvs'; eqvs; qbody } *)

(* (\* let hide_vars_in_ctx ctx vars ty = *\) *)
(* (\*   List.fold_right *\) *)
(* (\*     (fun id { uqvs; eqvs; k } -> *\) *)
(* (\*       let idty = *\) *)
(* (\*         match Typectx.get_opt ctx.k id with *\) *)
(* (\*         | None -> _failatwith __FILE__ __LINE__ "" *\) *)
(* (\*         | Some idty -> idty *\) *)
(* (\*       in *\) *)
(* (\*       { uqvs; eqvs; k = hide_quantify_variable_in_bodyt id idty k }) *\) *)
(* (\*     vars ty *\) *)

(* let empty = without_qv Typectx.UnderTypectx.empty *)

(* let fv f { uqvs; eqvs; k } = *)
(*   List.map (fun x -> x.x) uqvs @ List.map (fun x -> x.x) eqvs @ Typectx.fv f k *)
