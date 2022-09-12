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
  vc_head : P.t;
  vc_e_dts : string typed list;
  vc_body : P.t;
}

type vc_with_lemmas = {
  vcl_lemmas : P.t list;
  vcl_u_basics : string typed list;
  vcl_u_dts : string typed list;
  vcl_e_basics : string typed list;
  vcl_head : P.t;
  vcl_e_dts : string typed list;
  vcl_body : P.t;
}

type vc_with_lemmas_without_e_dts = {
  vclw_lemmas : P.t list;
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
  let l =
    List.filter_map
      (fun dt ->
        if eq udt.ty dt.ty then Some (P.subst_id prop udt.x dt.x) else None)
      udts
  in
  match l with [] -> None | _ -> Some (qvs, P.And l)

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
  match prop' with
  | [] -> (uqvs, prop)
  | _ ->
      if List.length uqvs > List.length uqvs' then
        uprop_merge_to_right (uqvs', prop') (uqvs, prop)
      else
        let prop =
          List.map (fun prop -> unify_to_vars (uqvs, prop) uqvs') prop
        in
        (uqvs', prop @ prop')

let rec u_union lemmas =
  match lemmas with
  | [] -> failwith "die"
  | [ (uqvs, prop) ] -> (uqvs, [ prop ])
  | (uqvs, prop) :: t ->
      let t = u_union t in
      uprop_merge_to_right (uqvs, [ prop ]) t
