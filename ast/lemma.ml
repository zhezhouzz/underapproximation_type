open Normalty.Ast
module P = Autov.Prop
module T = Termlang.T

type qmode = Qex | Qfa
type t = { qvs : (qmode * string Ntyped.typed) list; prop : P.t }

let to_prop { qvs; prop } =
  let open P in
  List.fold_right
    (fun (mode, qv) prop ->
      match mode with
      | Qex -> Exists (Ntyped.to_smttyped qv, prop)
      | Qfa -> Forall (Ntyped.to_smttyped qv, prop))
    qvs prop

let of_raw (qvs, prop) =
  let open T in
  let aux qv =
    match qv with
    | { x; ty = Some (Some "forall", nty) } -> (Qfa, Ntyped.{ x; ty = nty })
    | { x; ty = Some (Some "exists", nty) } -> (Qex, Ntyped.{ x; ty = nty })
    | { ty = Some (Some name, _); _ } ->
        failwith @@ Printf.sprintf "unknown label %s" name
    | { ty = Some (None, _); _ } -> failwith "wrong format"
    | _ -> failwith "wrong format: untyped"
  in
  { qvs = List.map aux qvs; prop }

open Ntyped

let assume_feprop { qvs; prop } =
  let uqvs, eqvs =
    List.fold_left
      (fun (uqvs, eqvs) (mode, qv) ->
        match mode with
        | Qfa ->
            if List.length eqvs != 0 then failwith "wrong format"
            else (uqvs @ [ qv ], eqvs)
        | Qex -> (uqvs, eqvs @ [ qv ]))
      ([], []) qvs
  in
  (uqvs, eqvs, prop)

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

let rec merge_to_right (uqvs, eqvs, prop) (uqvs', eqvs', prop') =
  if List.length uqvs > List.length uqvs' then
    merge_to_right (uqvs', eqvs', prop') (uqvs, eqvs, prop)
  else
    let prop = unify_to_vars (uqvs, prop) uqvs' in
    let eqvs, prop = rename_with_vars (eqvs, prop) in
    (uqvs', eqvs' @ eqvs, P.And [ prop; prop' ])

let rec union lemmas =
  match lemmas with
  | [] -> failwith "die"
  | [ h ] -> assume_feprop h
  | h :: t ->
      let uqvs, eqvs, prop = assume_feprop h in
      merge_to_right (uqvs, eqvs, prop) @@ union t

open Zzdatatype.Datatype

let instantiate (uqvs, eqvs, prop) uchoices =
  let open SMTtyped in
  let () = if List.length eqvs != 0 then failwith "die" else () in
  let uqvs_settings =
    List.map
      (fun x -> List.filter (fun y -> Autov.Smtty.eq y.ty x.ty) uchoices)
      uqvs
  in
  let settings = List.choose_list_list uqvs_settings in
  let unify_to_vars (vars, prop) vars' =
    let rec aux prop = function
      | [], _ -> prop
      | h :: t, h' :: t' -> aux (Autov.Prop.subst_id prop h.x h'.x) (t, t')
      | _ -> failwith "die"
    in
    aux prop (vars, vars')
  in
  let props =
    List.map (fun uqvs' -> unify_to_vars (uqvs, prop) uqvs') settings
  in
  P.And props

let with_lemma lemmas query uchoices =
  let uqvs, eqvs, prop = union lemmas in
  let uqvs = List.map to_smttyped uqvs in
  let eqvs = List.map to_smttyped eqvs in
  P.Implies (instantiate (uqvs, eqvs, prop) uchoices, query)
