open Languages
module P = Autov.Prop
module Typectx = MustMayTypectx
open Zzdatatype.Datatype
open Sugar
open Ntyped
open Abstraction
open Underctx

let with_lemma_to_query lemmas x =
  let pre, a, b = Lemma.query_with_lemma_to_prop @@ Lemma.with_lemma lemmas x in
  (* let () = Lemma.print_with_lemma (pre, b) in *)
  (* let () = failwith "end" in *)
  (pre, a, b)

let typed_to_smttyped = Languages.Ntyped.to_smttyped

let check file line pres q =
  match Autov.check pres q with
  | None -> ()
  | Some m ->
      (* let _ = Autov.Func_interp.get_preds_interp m in *)
      Autov._failwithmodel file line "Subtyping check: rejected by the verifier"
        m

let do_check file line (final_uqvs, final_eqvs, final_pre, final_post) =
  (* HACK: we do not really check unit *)
  if
    List.length
      (List.filter (fun x -> match x.ty with Ty_unit -> true | _ -> false)
      @@ final_uqvs @ final_eqvs)
    != 0
  then ()
  else
    let () =
      let hol_q =
        P.(
          topu_to_prop
            ( final_uqvs,
              tope_to_prop (final_eqvs, Implies (final_pre, final_post)) ))
      in
      match P.fv hol_q with
      | [] -> ()
      | fvs ->
          (* let () = Printf.printf "Q: %s\n" (Autov.pretty_layout_prop hol_q) in *)
          _failatwith __FILE__ __LINE__ @@ StrList.to_string fvs
    in
    let pres, uqvs, q =
      with_lemma_to_query (Prim.lemmas_to_pres ())
        (final_uqvs, final_eqvs, final_pre, final_post)
    in
    match
      List.substract String.equal (Autov.prop_fv q)
        (List.map (fun x -> x.x) uqvs)
    with
    | [] -> check file line pres q
    | fv ->
        let () = Printf.printf "q: %s\n" @@ Autov.pretty_layout_prop q in
        _failatwith __FILE__ __LINE__
          (spf "FV: %s" @@ Zzdatatype.Datatype.StrList.to_string fv)

(* let counter = ref 0 *)

let do_solve_pres pres =
  let rec aux = function
    | (id, ot) :: pres ->
        let ot_uqvs, ot_pre = aux pres in
        (* if List.exists (String.equal id) (P.fv ot_pre @ P.fv target) then *)
        if NT.is_basic_tp ot.UT.normalty then
          let x = { x = id; ty = ot.UT.normalty } in
          let prop = P.subst_id ot.prop ot.basename x.x in
          (x :: ot_uqvs, P.And [ prop; ot_pre ])
        else _failatwith __FILE__ __LINE__ ""
        (* else (ot_uqvs, ot_pre) *)
    | [] -> ([], P.mk_true)
  in
  let ot_uqvs, ot_pre = aux pres in
  (ot_uqvs, P.peval ot_pre)

let solve_pres file line pres (t1, t2) =
  let name1, nt1, prop1 = UT.assume_base __FILE__ __LINE__ t1 in
  let name2, nt2, prop2 = UT.assume_base __FILE__ __LINE__ t2 in
  let nt = _check_equality __FILE__ __LINE__ NT.eq nt1 nt2 in
  let typeself, prop1, prop2 =
    if String.equal name1 name2 then (name1, prop1, prop2)
    else (name1, P.peval prop1, P.subst_id prop2 name2 name1)
  in
  let ot_uqvs, ot_pre = do_solve_pres pres in
  let prop1 = P.peval prop1 in
  let prop2 = P.peval prop2 in
  let nu = { ty = nt; x = typeself } in
  let () =
    Typectx.pretty_print_q
      (List.map (fun x -> x.x) ot_uqvs @ [ nu.x ])
      []
      (And [ ot_pre; prop2 ])
      prop1
  in
  let eq2, pre2 = P.assume_tope_uprop __FILE__ __LINE__ prop2 in
  let final_eqvs, final_post =
    P.assume_tope_uprop_fresh_name __FILE__ __LINE__ prop1
  in
  let final_pre = P.And [ ot_pre; pre2 ] in
  let final_uqvs = ot_uqvs @ (nu :: eq2) in
  do_check file line (final_uqvs, final_eqvs, final_pre, final_post)

let check_in name t = List.exists (String.equal name) @@ UT.fv t

let check_under_ctx file line ctx (t1, t2) =
  let force_add (id, uty) t1 =
    if UT.is_base_type uty then UT.retty_add_ex_uprop_always_add (id, uty) t1
    else t1
  in
  let rec aux pres ctx (t1, t2) =
    match Typectx.destrct_right ctx with
    | None -> (pres, (t1, t2))
    | Some (ctx, (id, MMT.Ot oty)) -> aux ([ (id, oty) ] @ pres) ctx (t1, t2)
    | Some (ctx, (id, MMT.Consumed (UtCopy id')))
    | Some (ctx, (id, MMT.Ut (UtCopy id'))) ->
        let t1 = UT.subst_id t1 id id'.x in
        let t2 = UT.subst_id t2 id id'.x in
        aux pres ctx (t1, t2)
    | Some (ctx, (id, MMT.Consumed (UtNormal uty)))
    | Some (ctx, (id, MMT.Ut (UtNormal uty))) ->
        let t1 = force_add (id, uty) t1 in
        let t2 = force_add (id, uty) t2 in
        (* let t2 = match UT.fv t2 with [] -> t2 | _ -> force_add (id, uty) t2 in *)
        aux pres ctx (t1, t2)
    (* (match (check_in id t1, check_in id t2) with *)
    (* | false, false -> *)
    (*     (\* let () = *\) *)
    (*     (\*   Printf.printf "Add (%s, %s) -> %s\n" id (UT.pretty_layout uty) *\) *)
    (*     (\*     (UT.pretty_layout t1) *\) *)
    (*     (\* in *\) *)
    (*     let t1 = *)
    (*       if UT.is_base_type uty then *)
    (*         UT.retty_add_ex_uprop_always_add (id, uty) t1 *)
    (*       else t1 *)
    (*     in *)
    (*     aux pres ctx (t1, t2) *)
    (* | _, _ -> *)
    (*     let t1 = UT.retty_add_ex_uprop_drop_independent (id, uty) t1 in *)
    (*     let t2 = UT.retty_add_ex_uprop_drop_independent (id, uty) t2 in *)
    (*     aux pres ctx (t1, t2)) *)
  in
  let pres, (t1, t2) = aux [] ctx (t1, t2) in
  solve_pres file line pres (t1, t2)

let subtyping_check_ot_ file line ctx t1 t2 =
  let () = Pp.printf "@{<bold>OVERCHECK@}\n" in
  let () = Typectx.pretty_print_subtyping ctx (MMT.Ot t1, MMT.Ot t2) in
  let () = Pp.printf "@{<bold>OVERCHECK Converted@}\n" in
  let t1' = UT.ot_to_ut t1 in
  let t2' = UT.ot_to_ut t2 in
  let () =
    Typectx.pretty_print_subtyping ctx
      (MMT.Ut (MMT.UtNormal t2'), MMT.Ut (MMT.UtNormal t1'))
  in
  check_under_ctx file line ctx (t2', t1')

(* let subtyping_check_ot_ file line ctx t1 t2 = *)
(*   let rec aux pres ctx (t1, t2) = *)
(*     match Typectx.destrct_right ctx with *)
(*     | None -> (pres, (t1, t2)) *)
(*     | Some (ctx, (_, MMT.Consumed _)) -> aux pres ctx (t1, t2) *)
(*     | Some (ctx, (id, MMT.Ot oty)) -> aux ([ (id, oty) ] @ pres) ctx (t1, t2) *)
(*     | Some (ctx, (_, MMT.Ut _)) -> aux pres ctx (t1, t2) *)
(*   in *)
(*   let pres, (t1, t2) = aux [] ctx (t1, t2) in *)
(*   let () = *)
(*     let () = Pp.printf "@{<bold>OVERCHECK@}\n" in *)
(*     let psudo_ctx = List.map (fun (id, ty) -> (id, MMT.Ot ty)) pres in *)
(*     let () = Typectx.pretty_print_subtyping psudo_ctx (MMT.Ot t1, MMT.Ot t2) in *)
(*     () *)
(*   in *)
(*   solve_pres file line pres (UT.ot_to_ut t2, UT.ot_to_ut t1) *)

let subtyping_check_ file line (ctx : Typectx.ctx) (inferred_ty : UT.t)
    (target_ty : UT.t) =
  let open UT in
  (* let () = if !counter == 1 then failwith "end" else counter := !counter + 1 in *)
  let () =
    Typectx.pretty_print_subtyping ctx
      (MMT.(Ut (UtNormal inferred_ty)), MMT.(Ut (UtNormal target_ty)))
  in
  let rec aux ctx (t1, t2) =
    match (t1, t2) with
    | UnderTy_base _, UnderTy_base _ -> check_under_ctx file line ctx (t1, t2)
    | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
        List.iter (check_under_ctx file line ctx)
        @@ _safe_combine __FILE__ __LINE__ ts1 ts2
    | ( UnderTy_under_arrow { argty = t11; retty = t12 },
        UnderTy_under_arrow { argty = t21; retty = t22 } ) ->
        aux ctx (t21, t11);
        aux ctx (t12, t22)
    | ( UnderTy_over_arrow { argname = x1; argty = t11; retty = t12 },
        UnderTy_over_arrow { argname = x2; argty = t21; retty = t22 } ) ->
        let () = subtyping_check_ot_ file line ctx t21 t11 in
        let x' = Rename.unique x2 in
        let t12 = subst_id t12 x1 x' in
        let t22 = subst_id t22 x2 x' in
        let ctx = Typectx.ot_add_to_right ctx (x', t21) in
        aux ctx (t12, t22)
    | _, _ -> _failatwith __FILE__ __LINE__ "die: under subtype"
  in
  aux ctx (inferred_ty, target_ty)

let subtyping_check file line (ctx : Typectx.ctx) (inferred_ty : UT.t)
    (target_ty : UT.t) =
  try subtyping_check_ file line ctx inferred_ty target_ty with
  | Autov.FailWithModel (msg, m) ->
      let () = Pp.printf "@{<orange>Under Type Check failed:@}%s\n" msg in
      raise (FailwithCex (msg, m))
  | Autov.SMTTIMEOUT ->
      let () = Pp.printf "@{<orange>Under Type Check failed:@}%s\n" "timeout" in
      raise (FailTimeout (__FILE__, __LINE__))
  | e -> raise e

let subtyping_check_ot file line (ctx : Typectx.ctx) (inferred_ty : UT.ot)
    (target_ty : UT.ot) =
  try subtyping_check_ot_ file line ctx inferred_ty target_ty with
  | Autov.FailWithModel (msg, m) ->
      let () = Pp.printf "@{<orange>Over Type Check failed:@}%s\n" msg in
      raise (FailwithCex (msg, m))
  | Autov.SMTTIMEOUT ->
      let () = Pp.printf "@{<orange>Over Type Check failed:@}%s\n" "timeout" in
      raise (FailTimeout (__FILE__, __LINE__))
  | e -> raise e

let type_err_to_false f =
  try
    let _ = f () in
    true
  with
  | FailwithCex _ | FailUnderAgainstOver _ | FailOverAgainstUnder _
  | FailTimeout _ ->
      false
  | FailTypeConsumedonsumed _ ->
      let () = Pp.printf "@{<orange>Over Type Check failed:@}%s\n" "consumed" in
      false
  | e -> raise e

let subtyping_check_bool file line (ctx : Typectx.ctx) (inferred_ty : UT.t)
    (target_ty : UT.t) =
  type_err_to_false (fun () ->
      subtyping_check file line ctx inferred_ty target_ty)

let ot_subtyping_check_bool file line (ctx : Typectx.ctx) (inferred_ty : UT.ot)
    (target_ty : UT.ot) =
  type_err_to_false (fun () ->
      subtyping_check_ot file line ctx inferred_ty target_ty)

let mmt_check file line ctx t1 t2 =
  let open MMT in
  match (t1, t2) with
  | Consumed _, _ -> _err_consumed file line "??"
  | _, Consumed _ -> _err_consumed file line "??"
  | Ut (UtNormal t1), Ut (UtNormal t2) -> subtyping_check file line ctx t1 t2
  | Ut _, Ut _ -> _failatwith __FILE__ __LINE__ "never happen"
  | Ut _, Ot _ -> raise (FailUnderAgainstOver (file, line))
  | Ot _, Ut _ -> raise (FailOverAgainstUnder (file, line))
  | Ot t1, Ot t2 -> subtyping_check_ot file line ctx t1 t2

let mmt_check_bool file line ctx t1 t2 =
  type_err_to_false (fun () -> mmt_check file line ctx t1 t2)
