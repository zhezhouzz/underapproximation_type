open Ast
open Sugar
open Zzdatatype.Datatype
open Normalty.Ast.Ntyped
open Peval

let tope_to_prop (eqvs, prop) =
  List.fold_right (fun eqv prop -> Exists (eqv, prop)) eqvs prop

let topu_to_prop (uqvs, prop) =
  List.fold_right (fun eqv prop -> Forall (eqv, prop)) uqvs prop

let assume_fe file line prop =
  let rec aux = function
    | Forall (u, e) ->
        let eqs, e = aux e in
        (u :: eqs, e)
    | e ->
        if has_qv e then _failatwith file line "not a forall quantified"
        else ([], e)
  in
  aux prop

let assume_ee file line prop =
  let rec aux = function
    | Exists (u, e) ->
        let eqs, e = aux e in
        (u :: eqs, e)
    | e ->
        if has_qv e then
          _failatwith file line
            (spf "%s not a exist quantified" @@ Frontend.layout e)
        else ([], e)
  in
  aux prop

let rename_destruct_uprop_ (ids, prop) =
  List.fold_right
    (fun id (ids', prop) ->
      let id' = map Rename.unique id in
      (id' :: ids', subst_id prop id.x id'.x))
    ids ([], prop)

let rename_prop prop l =
  List.fold_right (fun (id, id') prop -> subst_id prop id.x id'.x) l prop

let rec conjunct_two_uprops_ (uqv1, prop1) (uqv2, prop2) =
  if List.length uqv2 < List.length uqv1 then
    conjunct_two_uprops_ (uqv2, prop2) (uqv1, prop1)
  else
    let uqv1' =
      List.sublist uqv2 ~start_included:0 ~end_excluded:(List.length uqv1)
    in
    let prop1 = rename_prop prop1 @@ List.combine uqv1 uqv1' in
    (uqv2, And [ prop1; prop2 ])

let lift_uprop file line prop =
  let rec aux prop =
    match prop with
    | Lit _ | MethodPred (_, _) | Implies (_, _) | Not _ | Ite (_, _, _) | Iff _
      ->
        if has_qv prop then _failatwith file line "" else ([], prop)
    | And es ->
        let uqvs, e =
          match List.map aux es with
          | [] -> ([], Lit (ACbool true))
          | [ h ] -> h
          | h :: t ->
              List.fold_left (fun (a, b) -> conjunct_two_uprops_ (a, b)) h t
        in
        (uqvs, peval e)
    | Or es ->
        let uqvs, es =
          List.split @@ List.map (fun e -> rename_destruct_uprop_ @@ aux e) es
        in
        (List.concat uqvs, peval (Or es))
    | Forall (u, e) ->
        let uqvs, e = aux e in
        (u :: uqvs, e)
    | Exists (_, _) -> _failatwith file line (Frontend.layout prop)
  in
  aux prop

let assume_tope_uprop file line prop =
  let rec aux = function
    | Exists (u, e) ->
        let eqs, e = aux e in
        (u :: eqs, e)
    | e ->
        if only_uq e then ([], e)
        else _failatwith file line (Frontend.layout prop)
  in
  aux prop

let conjunct_eprop_to_right_ (eqv1, prop1) (eqv2, prop2) =
  let is_eq id = function
    | MethodPred ("==", [ AVar x; ACint n ]) when String.equal x.x id.x ->
        Some (ACint n)
    | MethodPred ("==", [ ACint n; AVar x ]) when String.equal x.x id.x ->
        Some (ACint n)
    | _ -> None
  in
  (* let is_eq _ _ = None in *)
  let merge (eqv1, prop1) (eqv2, prop2) =
    let eqvs, prop1 =
      List.fold_right
        (fun eq (eqv2, prop1) ->
          if List.exists (fun y -> String.equal eq.x y.x) eqv2 then
            let eq' = map Rename.unique eq in
            (eq' :: eqv2, rename_prop prop1 [ (eq, eq') ])
          else (eq :: eqv2, prop1))
        eqv1 (eqv2, prop1)
    in
    tope_to_prop (eqvs, And [ prop1; prop2 ])
  in
  match eqv1 with
  | [ x ] -> (
      match is_eq x prop1 with
      | Some y ->
          let prop2' = subst_id_with_lit prop2 x.x y in
          (* let () = *)
          (*   Printf.printf "SIMP: %s ---> %s in %s => %s\n" x.x *)
          (*     (Frontend.pretty_layout_lit y) *)
          (*     (Frontend.pretty_layout prop2) *)
          (*     (Frontend.pretty_layout prop2') *)
          (* in *)
          tope_to_prop (eqv2, prop2')
      | _ -> merge (eqv1, prop1) (eqv2, prop2))
  | _ -> merge (eqv1, prop1) (eqv2, prop2)

let conjunct_eprop_to_right file line prop1 prop2 =
  conjunct_eprop_to_right_
    (assume_tope_uprop file line prop1)
    (assume_tope_uprop file line prop2)

let assume_tope_uprop_fresh_name file line prop =
  let eqvs, prop = assume_tope_uprop file line prop in
  let eqvs' = List.map (fun x -> map Rename.unique x) eqvs in
  let prop = rename_prop prop @@ List.combine eqvs eqvs' in
  (eqvs', prop)

let conjunct_eprops_ es =
  let eqvs, es =
    List.split
    @@ List.map
         (fun (eqv2, prop2) ->
           let eqv2' = List.map (fun x -> map Rename.unique x) eqv2 in
           let prop2 = rename_prop prop2 @@ List.combine eqv2 eqv2' in
           (eqv2', prop2))
         es
  in
  (List.concat eqvs, peval @@ And es)

(* TODO:simplify *)
let disjunct_eprops_ es =
  let eqvs, es =
    List.split
    @@ List.map
         (fun (eqv2, prop2) ->
           let eqv2' = List.map (fun x -> map Rename.unique x) eqv2 in
           let prop2 = rename_prop prop2 @@ List.combine eqv2 eqv2' in
           (eqv2', prop2))
         es
  in
  (List.concat eqvs, peval @@ Or es)

let conjunct_tope_uprop file line props =
  let props = List.map (assume_tope_uprop file line) props in
  tope_to_prop (conjunct_eprops_ props)

let disjunct_tope_uprop file line props =
  let props = List.map (assume_tope_uprop file line) props in
  tope_to_prop (disjunct_eprops_ props)
