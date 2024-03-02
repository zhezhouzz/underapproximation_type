include Frontendu.Typectx
include MustMayTypectx
open Sugar
open MMT
open Ntyped

let subtract ctx ctx' =
  let rec aux = function
    | ctx, [] -> ctx
    | [], _ -> _failatwith __FILE__ __LINE__ ""
    | (name, ty) :: ctx, (name', ty') :: ctx' ->
        if String.equal name name' && MMT.eq ty ty' then aux (ctx, ctx')
        else _failatwith __FILE__ __LINE__ ""
  in
  aux (ctx, ctx')

let destrct_right ctx =
  match List.rev ctx with [] -> None | h :: t -> Some (List.rev t, h)

let get_by_nt (ctx : ctx) nt =
  List.filter_map
    (fun (name, ty) ->
      if eq nt (erase ty) then Some ({ x = name; ty = nt }, ty) else None)
    ctx

let consume ctx name =
  let rec aux res = function
    | [] -> _failatwith __FILE__ __LINE__ ""
    | (name', ty) :: rest ->
        if String.equal name name' then
          match ty with
          | Ut ty -> res @ ((name', Consumed ty) :: rest)
          | Ot _ -> _failatwith __FILE__ __LINE__ ""
          | Consumed _ -> _failatwith __FILE__ __LINE__ ""
        else aux (res @ [ (name', ty) ]) rest
  in
  aux [] ctx

let check_appear_in_rest name ctx =
  List.iter
    (fun (name', ty) ->
      if String.equal name name' then
        _failatwith __FILE__ __LINE__ "alpha renaming error"
      else if List.exists (String.equal name) @@ MMT.fv ty then
        _failatwith __FILE__ __LINE__ "inv broken"
      else ())
    ctx

(* HACK *)
let extract ctx name =
  let rec aux res = function
    | [] -> _failatwith __FILE__ __LINE__ ""
    | (name', ty) :: rest ->
        if String.equal name name' then
          match ty with
          | Ut (UtNormal ty) ->
              let () = check_appear_in_rest name' rest in
              let ctx = res @ rest in
              (ctx, ty)
          | Ut (UtCopy _) -> _failatwith __FILE__ __LINE__ ""
          | Ot _ -> _failatwith __FILE__ __LINE__ ""
          | Consumed _ -> _failatwith __FILE__ __LINE__ ""
        else aux (res @ [ (name', ty) ]) rest
  in
  aux [] ctx

let try_simplify_measurement var prop =
  let measure = Env.get_measure () in
  let is_valid = ref true in
  let open Autov.Prop in
  let rec aux t =
    match t with
    | Not _ -> t
    | Lit _ | Implies (_, _) | Ite (_, _, _) | Or _ | Iff (_, _) ->
        is_valid := false;
        t
    | And es -> And (List.map aux es)
    | MethodPred (mp, args) -> (
        let args =
          List.filter_map
            (fun x -> match x with AVar x -> Some x | _ -> None)
            args
        in
        match
          (String.equal measure mp, List.exists (Ntyped.typed_eq var) args)
        with
        | true, true -> mk_true
        | true, false -> t
        | false, true ->
            is_valid := false;
            t
        | false, false -> t)
    | Forall (_, _) -> _failatwith __FILE__ __LINE__ "never happen"
    | Exists (_, _) -> _failatwith __FILE__ __LINE__ "never happen"
  in
  let prop' = aux prop in
  if !is_valid then
    let () =
      Env.show_debug_debug @@ fun _ ->
      Printf.printf "before: %s\n" @@ Autov.pretty_layout_prop prop
    in
    let () =
      Env.show_debug_debug @@ fun _ ->
      Printf.printf "after: %s\n" @@ Autov.pretty_layout_prop prop'
    in
    (* let _ = failwith "end" in *)
    Some prop'
  else None

let try_simplify_true_dec prop =
  let open Autov.Prop in
  let rec aux t =
    match t with
    | Not _ -> false
    | Lit (ACbool b) -> b
    | Lit _ -> false
    | Implies (_, _) | Ite (_, _, _) | Or _ | Iff (_, _) | MethodPred (_, _) ->
        false
    | And es -> List.for_all aux es
    | Forall (_, _) -> _failatwith __FILE__ __LINE__ "never happen"
    | Exists (_, _) -> _failatwith __FILE__ __LINE__ "never happen"
  in
  aux prop

let simplify_ut ut =
  (* let measure = Env.get_measure () in *)
  let open UT in
  match ut with
  | UnderTy_base { basename; normalty; prop } -> (
      let open Autov.Prop in
      let res = try Some (to_e_nf prop) with _ -> None in
      match res with
      | None -> ut
      | Some (eqvs, body) ->
          (* let () = *)
          (*   Printf.printf "simplify_ut: %s\n" (Autov.pretty_layout_prop body) *)
          (* in *)
          let prop =
            if try_simplify_true_dec body then mk_true
            else
              let new_eqvs, new_body =
                List.fold_right
                  (fun eqv (new_eqvs, new_body) ->
                    (* let v = { x = basename; ty = normalty } in *)
                    (* let clauze = mk_mp_vars measure [ v; eqv ] in *)
                    match try_simplify_measurement eqv new_body with
                    | None -> (eqv :: new_eqvs, new_body)
                    | Some new_body -> (new_eqvs, new_body))
                  eqvs ([], body)
              in
              P.tope_to_prop (new_eqvs, new_body)
          in
          UnderTy_base { basename; normalty; prop })
  | _ -> ut

let ut_force_add_to_right ctx (id, ty) =
  let ty =
    match ty with MMT.UtNormal ut -> MMT.UtNormal (simplify_ut ut) | _ -> ty
  in
  add_to_right ctx (id, Ut ty)

let ut_force_add_to_rights ctx ids =
  List.fold_left (fun ctx id -> ut_force_add_to_right ctx id) ctx ids

let ot_add_to_right ctx (id, ot) = add_to_right ctx (id, Ot ot)

let ot_add_to_rights ctx ids =
  List.fold_left (fun ctx id -> ot_add_to_right ctx id) ctx ids

(* Assume everything in the type context is not bot *)
let close_by_diff_ diff uty =
  List.fold_right
    (fun (x, xty) uty ->
      match xty with
      | UtNormal xty -> UT.retty_add_ex_uprop_always_add (x, xty) uty
      | UtCopy id -> UT.subst_id uty x id.x)
    diff uty

let close_by_diff ctx ctx' uty =
  let diff = subtract ctx ctx' in
  let diff =
    List.filter_map
      (function
        | x, Ut ut -> Some (x, ut)
        | x, Consumed ut -> Some (x, ut)
        | _, Ot _ -> _failatwith __FILE__ __LINE__ "")
      diff
  in
  close_by_diff_ diff uty

let check_in x p = List.exists (String.equal x) @@ Autov.prop_fv p
