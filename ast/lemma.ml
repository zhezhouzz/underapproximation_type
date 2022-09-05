open Normalty.Ast
module P = Autov.Prop
module T = Termlang.T
open Q
open Sugar
open Ntyped

type qprop = { mode : Q.t; qvs : string typed list; prop : P.t }
type t = { udt : string typed; qprop : qprop }

let split_to_u_e l = List.partition (fun x -> is_forall x.qprop.mode) l

let qprop_to_prop { mode; qvs; prop } =
  let open P in
  List.fold_right
    (fun qv prop ->
      if is_forall mode then Forall (qv, prop) else Exists (qv, prop))
    qvs prop

let to_prop { udt; qprop } =
  let open P in
  Forall (udt, qprop_to_prop qprop)

let parse_qvs qvs =
  let aux qv =
    match qv with
    | T.{ x; ty = Some (Some "forall", nty) } -> (Q.Fa, { x; ty = nty })
    | T.{ x; ty = Some (Some "exists", nty) } -> (Q.Ex, { x; ty = nty })
    | T.{ ty = Some (Some name, _); _ } ->
        _failatwith __FILE__ __LINE__ @@ Printf.sprintf "unknown label %s" name
    | T.{ ty = Some (None, _); _ } -> failwith "wrong format"
    | _ -> _failatwith __FILE__ __LINE__ "wrong format: untyped"
  in
  List.map aux qvs

let of_raw (qvs, prop) =
  match parse_qvs qvs with
  | [] -> _failatwith __FILE__ __LINE__ ""
  | (Q.Fa, udt) :: qvs ->
      if not (NT.is_dt udt.ty) then _failatwith __FILE__ __LINE__ ""
      else
        let to_qvs qvs = List.map snd qvs in
        let qprop =
          if List.for_all (fun (mode, _) -> is_forall mode) qvs then
            { mode = Q.Fa; qvs = to_qvs qvs; prop }
          else if List.for_all (fun (mode, _) -> is_exists mode) qvs then
            { mode = Q.Ex; qvs = to_qvs qvs; prop }
          else _failatwith __FILE__ __LINE__ ""
        in
        { udt; qprop }
  | _ -> _failatwith __FILE__ __LINE__ ""

type vc = {
  vc_u_basics : string typed list;
  vc_u_dts : string typed list;
  vc_e_basics : string typed list;
  vc_e_dts : string typed list;
  vc_body : P.t;
}

type vc_with_lemmas = {
  vcl_pres : P.t list;
  vcl_u_basics : string typed list;
  vcl_u_dts : string typed list;
  vcl_e_basics : string typed list;
  vcl_e_dts : string typed list;
  vcl_body : P.t;
}

type vc_with_lemmas_without_e_dts = {
  vclw_pres : P.t list;
  vclw_u_basics : string typed list;
  vclw_u_dts : string typed list;
  vclw_e_basics : string typed list;
  vclw_body : P.t;
}

let qprop_subst_id { mode; qvs; prop } id id' =
  if List.exists (fun x -> String.equal x.x id) qvs then
    _failatwith __FILE__ __LINE__ ""
  else { mode; qvs; prop = P.subst_id prop id id' }

let instantiate_dt { udt; qprop = { qvs; prop; _ } } udts =
  ( qvs,
    P.And
      (List.filter_map
         (fun dt ->
           if eq udt.ty dt.ty then Some (P.subst_id prop udt.x dt.x) else None)
         udts) )

let rename_with_vars (vars, prop) =
  List.fold_right
    (fun qv (vars, prop) ->
      let qv' = { x = Rename.unique qv.x; ty = qv.ty } in
      (qv' :: vars, Autov.Prop.subst_id prop qv.x qv'.x))
    vars ([], prop)

let unify_to_vars (vars, prop) vars' =
  let rec aux prop = function
    | [], _ -> prop
    | h :: t, h' :: t' -> aux (Autov.Prop.subst_id prop h.x h'.x) (t, t')
    | _ -> failwith "die"
  in
  aux prop (vars, vars')

let rec uprop_merge_to_right (uqvs, prop) (uqvs', prop') =
  if List.length uqvs > List.length uqvs' then
    uprop_merge_to_right (uqvs', prop') (uqvs, prop)
  else
    let prop = unify_to_vars (uqvs, prop) uqvs' in
    (uqvs', P.And [ prop; prop' ])

let rec u_union lemmas =
  match lemmas with
  | [] -> failwith "die"
  | [ h ] -> h
  | h :: t ->
      let t = u_union t in
      uprop_merge_to_right h t

let add_lemmas lemmas { vc_u_basics; vc_u_dts; vc_e_basics; vc_e_dts; vc_body }
    =
  let ulemmas, elemmas = split_to_u_e lemmas in
  let vcl_pres = List.map to_prop ulemmas in
  let elemmas = List.map (fun x -> instantiate_dt x vc_u_dts) elemmas in
  let vcl_u_basics, vcl_body =
    match elemmas with
    | [] -> (vc_u_basics, vc_body)
    | elemmas ->
        let u_basics', prop' = u_union elemmas in
        (vc_u_basics @ u_basics', P.Implies (prop', vc_body))
  in
  {
    vcl_pres;
    vcl_u_basics;
    vcl_u_dts = vc_u_dts;
    vcl_e_basics = vc_e_basics;
    vcl_e_dts = vc_e_dts;
    vcl_body;
  }

let without_e_dt lemmas
    { vcl_pres; vcl_u_basics; vcl_u_dts; vcl_e_basics; vcl_e_dts; vcl_body } =
  let _, _ = split_to_u_e lemmas in
  let vclw_e_basics', vclw_body =
    Autov.uqv_encoding (List.map (fun x -> x.x) vcl_e_dts) vcl_body
  in
  ( vcl_pres,
    List.fold_right (fun x prop -> P.Forall (x, prop)) (vcl_u_basics @ vcl_u_dts)
    @@ List.fold_right
         (fun x prop -> P.Exists (x, prop))
         (vcl_e_basics @ vclw_e_basics')
         vclw_body )

let with_lemma lemmas (uqvs, eqvs, vc_body) =
  let vc_u_dts, vc_u_basics = List.partition (fun x -> is_dt x.ty) uqvs in
  let vc_e_dts, vc_e_basics = List.partition (fun x -> is_dt x.ty) eqvs in
  let x =
    add_lemmas lemmas { vc_u_basics; vc_u_dts; vc_e_basics; vc_e_dts; vc_body }
  in
  without_e_dt lemmas x

(* open Ntyped *)

(* let assume_feprop { qvs; prop } = *)
(*   let uqvs, eqvs = *)
(*     List.fold_left *)
(*       (fun (uqvs, eqvs) (mode, qv) -> *)
(*         match mode with *)
(*         | Fa -> *)
(*             if List.length eqvs != 0 then failwith "wrong format" *)
(*             else (uqvs @ [ qv ], eqvs) *)
(*         | Ex -> (uqvs, eqvs @ [ qv ])) *)
(*       ([], []) qvs *)
(*   in *)
(*   (uqvs, eqvs, prop) *)

(* let rename_with_vars (vars, prop) = *)
(*   List.fold_right *)
(*     (fun qv (vars, prop) -> *)
(*       let qv' = { x = Rename.unique qv.x; ty = qv.ty } in *)
(*       (qv' :: vars, Autov.Prop.subst_id prop qv.x qv'.x)) *)
(*     vars ([], prop) *)

(* let unify_to_vars (vars, prop) vars' = *)
(*   let rec aux prop = function *)
(*     | [], _ -> prop *)
(*     | h :: t, h' :: t' -> aux (Autov.Prop.subst_id prop h.x h'.x) (t, t') *)
(*     | _ -> failwith "die" *)
(*   in *)
(*   aux prop (vars, vars') *)

(* let rec merge_to_right (uqvs, eqvs, prop) (uqvs', eqvs', prop') = *)
(*   if List.length uqvs > List.length uqvs' then *)
(*     merge_to_right (uqvs', eqvs', prop') (uqvs, eqvs, prop) *)
(*   else *)
(*     let prop = unify_to_vars (uqvs, prop) uqvs' in *)
(*     let eqvs, prop = rename_with_vars (eqvs, prop) in *)
(*     (uqvs', eqvs' @ eqvs, P.And [ prop; prop' ]) *)

(* let rec union lemmas = *)
(*   match lemmas with *)
(*   | [] -> failwith "die" *)
(*   | [ h ] -> assume_feprop h *)
(*   | h :: t -> *)
(*       let uqvs, eqvs, prop = assume_feprop h in *)
(*       merge_to_right (uqvs, eqvs, prop) @@ union t *)

(* open Zzdatatype.Datatype *)

(* let instantiate (uqvs, eqvs, prop) uchoices = *)
(*   let () = if List.length eqvs != 0 then failwith "die" else () in *)
(*   let uqvs_settings = *)
(*     List.map (fun x -> List.filter (fun y -> eq y.ty x.ty) uchoices) uqvs *)
(*   in *)
(*   let settings = List.choose_list_list uqvs_settings in *)
(*   let unify_to_vars (vars, prop) vars' = *)
(*     let rec aux prop = function *)
(*       | [], _ -> prop *)
(*       | h :: t, h' :: t' -> aux (Autov.Prop.subst_id prop h.x h'.x) (t, t') *)
(*       | _ -> failwith "die" *)
(*     in *)
(*     aux prop (vars, vars') *)
(*   in *)
(*   let props = *)
(*     List.map (fun uqvs' -> unify_to_vars (uqvs, prop) uqvs') settings *)
(*   in *)
(*   P.And props *)

(* let with_lemma lemmas query uchoices = *)
(*   let uqvs, eqvs, prop = union lemmas in *)
(*   P.Implies (instantiate (uqvs, eqvs, prop) uchoices, query) *)
