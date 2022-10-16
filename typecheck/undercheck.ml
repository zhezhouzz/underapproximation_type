open Languages
open Zzdatatype.Datatype
open Abstraction
open Sugar

(* include Litcheck *)
(* module P = Autov.Prop *)
include Checkaux

let current_rec_self : string option = None

let value_set_bot (a : NL.value NL.typed) =
  let open UL in
  { ty = UT.nt_to_exn_type (snd a.ty); x = Exn }

let term_set_bot (a : NL.term NL.typed) =
  let open UL in
  { ty = UT.nt_to_exn_type (snd a.ty); x = V Exn }

let reachability_check uctx ty = Reachability_check.reachability_check uctx ty
let persistence_check uctx ty = Persistence_check.persistence_check uctx ty

let rec value_type_infer (uctx : uctx) (a : NL.value NL.typed) :
    UL.value UL.typed =
  let aty = a.ty in
  let open UL in
  match a.x with
  | NL.Exn -> { ty = UT.nt_to_exn_type (snd aty); x = Exn }
  | NL.Lit lit ->
      typed_map (fun x -> Lit x) @@ lit_type_infer uctx { ty = aty; x = lit }
  | NL.Lam (id, rankfunc, body) -> (
      let typename =
        match fst id.ty with
        | None -> _failatwith __FILE__ __LINE__ "die"
        | Some x -> x
      in
      let idty =
        match Typectx.get_opt uctx.nctx typename with
        | Some (_, idty) -> idty
        | None ->
            _failatwith __FILE__ __LINE__
            @@ spf "cannot find the notation of %s (named %s)" id.x typename
      in
      match idty with
      | Consumed _ | NoRefinement _ ->
          _failatwith __FILE__ __LINE__ "wrong format"
      | Ut idty -> (
          let id = erase_check_mk_id __FILE__ __LINE__ id idty in
          match Typectx.ut_add_to_right reachability_check uctx.ctx id with
          | None ->
              let body = term_set_bot body in
              { ty = body.ty; x = Lam (id, rankfunc, body) }
          | Some ctx ->
              let body = term_type_infer { uctx with ctx } body in
              let ty =
                UT.UnderTy_under_arrow { argty = id.ty; retty = body.ty }
              in
              { ty; x = Lam (id, rankfunc, body) })
      | Ot idty ->
          let _ =
            _check_equality __FILE__ __LINE__ NT.eq idty.UT.normalty (snd id.ty)
          in
          let body =
            term_type_infer
              { uctx with ctx = Typectx.ot_add_to_right uctx.ctx (id.x, idty) }
              body
          in
          let ty =
            UT.UnderTy_over_arrow
              { argname = id.x; argty = idty; retty = body.ty }
          in
          let id = { x = id.x; ty = UT.make_basic_bot (snd id.ty) } in
          { ty; x = Lam (id, rankfunc, body) })
  | NL.Fix _ -> _failatwith __FILE__ __LINE__ "unimp"

and value_type_check (uctx : uctx) (a : NL.value NL.typed) (ty : UT.t) :
    UL.value UL.typed =
  let open UT in
  let result =
    match (a.NL.x, ty) with
    | NL.Exn, _ ->
        let x = value_type_infer uctx a in
        term_subtyping_check __FILE__ __LINE__ uctx x ty
    | NL.Lit _, _ ->
        let x = value_type_infer uctx a in
        term_subtyping_check __FILE__ __LINE__ uctx x ty
    (* | NL.Lam (_, _, _), UnderTy_ghost_arrow { argname; argty; retty } -> *)
    (*     value_type_check *)
    (*       { uctx with ctx = Typectx.ot_add_to_right uctx.ctx (argname, argty) } *)
    (*       a retty *)
    | NL.Lam (id, rankfunc, body), UnderTy_over_arrow { argname; argty; retty }
      ->
        let () =
          match Typectx.get_opt uctx.nctx id.x with
          | Some _ -> _failatwith __FILE__ __LINE__ "die"
          | None -> ()
        in
        let idty =
          _check_equality __FILE__ __LINE__ NTyped.eq argty.normalty
            (snd id.NL.ty)
        in
        let retty = UT.subst_id retty argname id.x in
        let body =
          term_type_check
            { uctx with ctx = Typectx.ot_add_to_right uctx.ctx (id.x, argty) }
            body retty
        in
        let id = UL.{ x = id.x; ty = UT.make_basic_bot idty } in
        UL.
          {
            ty = UnderTy_over_arrow { argname; argty; retty = body.ty };
            x = Lam (id, rankfunc, body);
          }
    | NL.Lam (id, rankfunc, body), UnderTy_under_arrow { argty; retty } -> (
        let () =
          match Nctx.get_opt uctx.nctx id.x with
          | Some _ -> _failatwith __FILE__ __LINE__ "die"
          | None -> ()
        in
        let id = erase_check_mk_id __FILE__ __LINE__ id argty in
        match Typectx.ut_add_to_right reachability_check uctx.ctx id with
        | None ->
            let body = term_set_bot body in
            let body = term_subtyping_check __FILE__ __LINE__ uctx body retty in
            { ty = body.ty; x = Lam (id, rankfunc, body) }
        | Some ctx ->
            let body = term_type_check { uctx with ctx } body retty in
            {
              ty = UnderTy_under_arrow { argty; retty = body.ty };
              x = Lam (id, rankfunc, body);
            })
    | NL.Lam (_, _, _), _ -> _failatwith __FILE__ __LINE__ ""
    | NL.Fix (f, _), ty ->
        let _ = erase_check_mk_id __FILE__ __LINE__ f ty in
        (* NOTE: no step 1 (full projection check) any more *)
        (* let _ = full_projection_check uctx ty in *)
        _failatwith __FILE__ __LINE__ "fix unimp"
    (* let uctx_base = *)
    (*   { *)
    (*     uctx with *)
    (*     ctx = *)
    (*       Typectx.norefinement_force_add_to_right uctx.ctx *)
    (*         { x = f.x; ty = snd @@ f.ty }; *)
    (*   } *)
    (* in *)
    (* let ty_base = right_ty_measure_0 uctx_base ty in *)
    (* let () = *)
    (*   Typectx.pretty_print_judge uctx_base.ctx *)
    (*     (NL.layout_value body, ty_base) *)
    (* in *)
    (* let _ = value_type_check uctx_base body ty_base in *)
    (* (\* let _ = failwith "end" in *\) *)
    (* let uctx_ind = *)
    (*   { *)
    (*     uctx with *)
    (*     ctx = Typectx.ut_force_add_to_right uctx.ctx UL.{ x = f.x; ty }; *)
    (*   } *)
    (* in *)
    (* let ty_ind = right_ty_measure_ind uctx_ind ty in *)
    (* let () = *)
    (*   Typectx.pretty_print_judge uctx_ind.ctx (NL.layout_value body, ty_ind) *)
    (* in *)
    (* let body = value_type_check uctx_ind body ty_ind in *)
    (* { ty; x = Fix ({ x = f.x; ty }, body) } *)
  in
  result

and handle_let_body (uctx : uctx) ctx' body target_type =
  let trivial_res =
    let body = term_set_bot body in
    body
  in
  match (ctx', target_type) with
  | None, None -> (trivial_res.ty, trivial_res)
  | Some ctx', None ->
      let body = term_type_infer { uctx with ctx = ctx' } body in
      (Typectx.close_by_diff ctx' uctx.ctx body.ty, body)
  | None, Some ty ->
      let res = term_subtyping_check __FILE__ __LINE__ uctx trivial_res ty in
      (res.ty, res)
  | Some ctx', Some ty ->
      let body = term_type_check { uctx with ctx = ctx' } body ty in
      (ty, body)

and handle_lettu (uctx : uctx) (tu, args, body) target_type =
  let open UL in
  let args = List.map (id_type_infer uctx) args in
  let tu =
    erase_check_mk_id __FILE__ __LINE__ tu
      (UT.UnderTy_tuple (List.map (fun x -> x.ty) args))
  in
  let ctx' = Typectx.ut_add_to_right reachability_check uctx.ctx tu in
  let ty, body = handle_let_body uctx ctx' body target_type in
  UL.{ ty; x = LetTu { tu; args; body } }

and handle_letdetu (uctx : uctx) (tu, args, body) target_type =
  let open UL in
  let tu = id_type_infer uctx tu in
  let rec loop = function
    | [], [] -> []
    | x :: args, ty :: ts ->
        erase_check_mk_id __FILE__ __LINE__ x ty :: loop (args, ts)
    | _ -> _failatwith __FILE__ __LINE__ ""
  in
  let args = loop (args, UT.assume_tuple __FILE__ __LINE__ tu.ty) in
  let ctx' = Typectx.ut_add_to_rights reachability_check uctx.ctx args in
  let ty, body = handle_let_body uctx ctx' body target_type in
  UL.{ ty; x = LetDeTu { tu; args; body } }

and handle_letapp (uctx : uctx) (ret, (_, fname), fty, args, body) target_type =
  let open UL in
  let open UT in
  (* arguments type check *)
  let args = List.map (id_type_infer uctx) args in
  let () = Typectx.pretty_print_app_judge fname uctx.ctx (args, Ut fty) in
  let rec aux uctx (a, b) : UT.t option =
    match (a, b) with
    (* | args, UnderTy_ghost_arrow { argty; argname; retty } -> ( *)
    (*     if if_rec && if_rec_measure_arg uctx argname then *)
    (*       let ghost_term = synthesize_ghost_term uctx in *)
    (*       let () = *)
    (*         Pp.printf "@{<bold> Ghost term type: %s:%s@}\n" ghost_term.x *)
    (*           (UT.pretty_layout ghost_term.ty) *)
    (*       in *)
    (*       (\* NOTE: reduce to normal over type check *\) *)
    (*       let uctx = *)
    (*         { *)
    (*           uctx with *)
    (*           ctx = Typectx.ut_force_add_to_right uctx.ctx ghost_term; *)
    (*         } *)
    (*       in *)
    (*       aux uctx *)
    (*         (ghost_term :: args, UnderTy_over_arrow { argty; argname; retty }) *)
    (*     else *)
    (*       let () = Pp.printf "@{<bold>GHOST: %s!@}\n" argname in *)
    (*       let candidates = candidate_vars_by_nt uctx argty.normalty in *)
    (*       (\* NOTE: the candiate cannot be the rest arguments. *\) *)
    (*       let candidates = *)
    (*         List.filter *)
    (*           (fun (x, _) -> *)
    (*             not (List.exists (fun y -> String.equal x.NTyped.x y.UL.x) args)) *)
    (*           candidates *)
    (*       in *)
    (*       let () = *)
    (*         Pp.printf "@{<bold>Check candidates: %s@}\n" *)
    (*           (List.split_by_comma (fun (x, _) -> x.NTyped.x) candidates) *)
    (*       in *)
    (*       let candidates = *)
    (*         List.filter *)
    (*           (fun (_, cty) -> *)
    (*             Undersub.subtyping_check_bool __FILE__ __LINE__ uctx.ctx *)
    (*               (ot_to_ut argty) cty) *)
    (*           candidates *)
    (*       in *)
    (*       (\* let candidates = *\) *)
    (*       (\*   List.filter *\) *)
    (*       (\*     (fun x -> *\) *)
    (*       (\*       String.equal x.Ntyped.x "lo" || String.equal x.Ntyped.x "hi") *\) *)
    (*       (\*     candidates *\) *)
    (*       (\* in *\) *)
    (*       let () = *)
    (*         Pp.printf "@{<bold>Got candidates: %s@}\n" *)
    (*           (List.split_by_comma (fun (x, _) -> x.NTyped.x) candidates) *)
    (*       in *)
    (*       let candidates = List.map fst candidates in *)
    (*       (\* let () = failwith "end" in *\) *)
    (*       let res = *)
    (*         List.filter_map *)
    (*           (fun candidate -> *)
    (*             let retty = subst_id retty argname candidate.Ntyped.x in *)
    (*             match aux uctx (args, retty) with *)
    (*             | None -> *)
    (*                 let () = *)
    (*                   Pp.printf "@{<bold>candidates %s has been rejected@}\n" *)
    (*                     candidate.Ntyped.x *)
    (*                 in *)
    (*                 None *)
    (*             | Some ty -> *)
    (*                 let () = *)
    (*                   Pp.printf "@{<bold>candidates %s has been accepted@}\n" *)
    (*                     candidate.Ntyped.x *)
    (*                 in *)
    (*                 Some (candidate, ty)) *)
    (*           candidates *)
    (*       in *)
    (*       match res with *)
    (*       | [] -> None *)
    (*       | [ (_, ty) ] -> Some ty *)
    (*       | ress -> *)
    (*           let () = *)
    (*             Pp.printf "succ candidates: %s\n" *)
    (*             @@ List.split_by_comma (fun (a, _) -> a.NTyped.x) ress *)
    (*           in *)
    (*           let ty = merge_case_tys @@ List.map snd ress in *)
    (*           Some ty *)
    (*       (\* _failatwith __FILE__ __LINE__ "ghost application multi succ" *\)) *)
    | [], ty -> Some ty
    | arg :: args, UnderTy_over_arrow { argname; argty; retty } ->
        (* NOTE: special application rule *)
        if
          Undersub.mmt_check_bool __FILE__ __LINE__ uctx.ctx
            (Ut (ot_to_ut argty))
            (Ut arg.UL.ty)
        then
          let retty = subst_id retty argname arg.x in
          aux uctx (args, retty)
        else None
    | arg :: args, UnderTy_under_arrow { retty; argty } -> (
        match term_subtyping_check_opt __FILE__ __LINE__ uctx arg argty with
        | None -> None
        | Some _ ->
            if persistence_check uctx arg.ty then aux uctx (args, retty)
            else
              let uctx = { uctx with ctx = Typectx.consume uctx.ctx arg.x } in
              let () = Pp.printf "@{<bold>Consume variable %s@}\n" arg.x in
              let () = Typectx.pretty_print uctx.ctx in
              aux uctx (args, retty))
    | _, _ -> _failatwith __FILE__ __LINE__ ""
  in
  let yty_opt = aux uctx (args, fty) in
  (* let () = Pp.printf "@{<bold>Begin@}\n" in *)
  match yty_opt with
  | None ->
      let retty = make_basic_bot (snd ret.NL.ty) in
      let ret = erase_check_mk_id __FILE__ __LINE__ ret retty in
      let () =
        Pp.printf "@{<bold>Let Body:@} %s => bottom type\n" (NL.layout body)
      in
      (* let ctx' = Typectx.ut_force_add_to_right uctx.ctx ret in *)
      let ty, body = handle_let_body uctx None body target_type in
      (ty, (ret, args, body))
  | Some retty ->
      let uctx' = handle_consume uctx (args, fty) in
      let ret = erase_check_mk_id __FILE__ __LINE__ ret retty in
      let () =
        Pp.printf "@{<bold>Let LHS:@} %s => %s\n" ret.x
          (UT.pretty_layout ret.ty)
      in
      let ctx' = Typectx.ut_add_to_right reachability_check uctx'.ctx ret in
      let ty, body = handle_let_body uctx' ctx' body target_type in
      (ty, (ret, args, body))

and handle_letval (uctx : uctx) (lhs, rhs, body) target_type =
  let open UL in
  let rhs = value_type_infer uctx rhs in
  let lhs = erase_check_mk_id __FILE__ __LINE__ lhs rhs.ty in
  let ctx' = Typectx.ut_force_add_to_right uctx.ctx lhs in
  (* NOTE: value is always reachable and persisitence *)
  let ty, body = handle_let_body uctx (Some ctx') body target_type in
  UL.{ ty; x = LetVal { lhs; rhs; body } }

and handle_ite uctx (cond, e_t, e_f) =
  let open UL in
  (* let () = *)
  (*   Pp.printf "@{<bold>Before If@}\n"; *)
  (*   Typectx.pretty_print uctx.ctx *)
  (* in *)
  let cond = id_type_infer uctx cond in
  let handle_case b e =
    let ty =
      UT.make_basic_from_prop NT.Ty_bool (fun x ->
          P.(if b then bvar_to_prop x else Not (bvar_to_prop x)))
    in
    match
      Typectx.ut_update reachability_check uctx.ctx
        (cond.x, fun ty' -> UT.conjunct ty ty')
    with
    | None -> term_set_bot e
    | Some ctx' ->
        let uctx' = { uctx with ctx = ctx' } in
        let e' = term_type_infer uctx' e in
        (* let () = Typectx.pretty_print_infer uctx'.ctx (NL.layout e, ty) in *)
        let ty = UT.retty_add_bool_eq e'.ty cond.x b in
        (* let () = Typectx.pretty_print_infer uctx'.ctx (NL.layout e, e'.ty) in *)
        (* let () = Typectx.pretty_print_infer uctx'.ctx (NL.layout e, ty) in *)
        { x = e'.x; ty }
  in
  let e_t = handle_case true e_t in
  let e_f = handle_case false e_f in
  let ty = merge_case_tys @@ List.map (fun e -> e.ty) [ e_t; e_f ] in
  { ty; x = Ite { cond; e_t; e_f } }

and handle_match uctx (matched, cases) =
  let open UL in
  let matched = id_type_infer uctx matched in
  let handle_case_one_ty NL.{ constructor; args; exp } constructor_ty =
    let constructor = UL.{ ty = constructor_ty; x = constructor.x } in
    let retty, args =
      let open UT in
      match constructor.ty with
      | UnderTy_base _ -> (constructor.ty, [])
      | UnderTy_under_arrow { argty; retty = UnderTy_tuple ts } ->
          ( argty,
            List.map (fun (x, ty) -> { x; ty })
            @@ _safe_combine __FILE__ __LINE__ args ts )
      | _ -> _failatwith __FILE__ __LINE__ "wrong rev under prim"
    in
    let trivial_res =
      let ty = UT.make_basic_bot (snd exp.NL.ty) in
      ( ty,
        UL.
          {
            constructor;
            args = List.map (fun x -> x.x) args;
            exp = term_set_bot exp;
          } )
    in
    match term_subtyping_check_opt __FILE__ __LINE__ uctx matched retty with
    | None -> trivial_res
    | Some _ -> (
        match
          Typectx.ut_update reachability_check uctx.ctx
            (matched.x, fun _ -> retty)
        with
        | None -> trivial_res
        | Some ctx' ->
            let ctx'' = Typectx.ut_force_add_to_rights ctx' args in
            let exp = term_type_infer { uctx with ctx = ctx'' } exp in
            ( Typectx.close_by_diff ctx'' ctx' exp.ty,
              UL.{ constructor; args = List.map (fun x -> x.x) args; exp } ))
  in
  let handle_case NL.{ constructor; args; exp } =
    let tys, cases =
      List.split
        (List.map (fun ty ->
             let ty = unify __FILE__ __LINE__ ty (snd constructor.ty) in
             handle_case_one_ty NL.{ constructor; args; exp } ty)
        @@ Prim.get_primitive_rev_under_ty (constructor.x, snd constructor.ty))
    in
    let case =
      match cases with [] -> _failatwith __FILE__ __LINE__ "" | h :: _ -> h
    in
    (merge_case_tys tys, case)
  in
  let tys, cases = List.split @@ List.map handle_case cases in
  (* { ty = List.nth tys 1; x = Match { matched; cases } } *)
  { ty = merge_case_tys tys; x = Match { matched; cases } }

and term_type_infer (uctx : uctx) (a : NL.term NL.typed) : UL.term UL.typed =
  let open NL in
  let res =
    match a.x with
    | V v ->
        UL.(typed_map (fun x -> V x))
        @@ value_type_infer uctx { ty = a.ty; x = v }
    | LetTu { tu; args; body } -> handle_lettu uctx (tu, args, body) None
    | LetDeTu { tu; args; body } -> handle_letdetu uctx (tu, args, body) None
    | LetOp { ret; op; args; body } ->
        let opty =
          Prim.get_primitive_under_ty
            ( Op.op_to_string op,
              NT.construct_arrow_tp
                (List.map (fun x -> snd x.ty) args, snd ret.ty) )
        in
        let fname = Op.op_to_string op in
        let ty, (ret, args, body) =
          handle_letapp uctx
            (ret, (if_rec_function uctx fname, fname), opty, args, body)
            None
        in
        { ty; x = LetOp { ret; op; args; body } }
    | LetApp { ret; f; args; body } -> (
        let f =
          try Some (id_type_infer uctx f)
          with FailTypeConsumedonsumed _ -> None
        in
        match f with
        | None -> term_set_bot a
        | Some f ->
            (* let () = Printf.printf "2: op = %s\n" (layout a) in *)
            let ty, (ret, args, body) =
              handle_letapp uctx
                (ret, (if_rec_function uctx f.x, f.x), f.ty, args, body)
                None
            in
            { ty; x = LetApp { ret; f; args; body } })
    | LetDtConstructor { ret; f; args; body } ->
        let fnty = recover_dt_constructor_ty (ret, args) in
        let fty = Prim.get_primitive_under_ty (f, snd fnty) in
        (* let () = Printf.printf "3: op = %s\n" (layout a) in *)
        let ty, (ret, args, body) =
          handle_letapp uctx
            (ret, (if_rec_function uctx f, f), fty, args, body)
            None
        in
        { ty; x = LetDtConstructor { ret; f; args; body } }
    | LetVal { lhs; rhs; body } -> handle_letval uctx (lhs, rhs, body) None
    | Ite { cond; e_t; e_f } -> handle_ite uctx (cond, e_t, e_f)
    | Match { matched; cases } -> handle_match uctx (matched, cases)
  in
  let () = Typectx.pretty_print_infer uctx.ctx (NL.layout a, res.UL.ty) in
  res

and term_type_check (uctx : uctx) (x : NL.term NL.typed) (ty : UT.t) :
    UL.term UL.typed =
  let () = Typectx.pretty_print_judge uctx.ctx (NL.layout x, ty) in
  let _ = _check_equality __FILE__ __LINE__ NT.eq (UT.erase ty) (snd @@ x.ty) in
  let open NL in
  match (x.x, ty) with
  | V v, _ ->
      let v = value_type_check uctx { ty = x.ty; x = v } ty in
      { ty = v.ty; x = V v.x }
  (* | _, UT.(UnderTy_ghost_arrow { argname; argty; retty }) -> *)
  (*     term_type_check *)
  (*       { uctx with ctx = Typectx.ot_add_to_right uctx.ctx (argname, argty) } *)
  (*       x retty *)
  | LetTu { tu; args; body }, _ -> handle_lettu uctx (tu, args, body) (Some ty)
  | LetDeTu { tu; args; body }, _ ->
      handle_letdetu uctx (tu, args, body) (Some ty)
  | LetApp { ret; f; args; body }, _ -> (
      let f =
        try Some (id_type_infer uctx f) with FailTypeConsumedonsumed _ -> None
      in
      match f with
      | None ->
          let x = term_set_bot x in
          let x = term_subtyping_check __FILE__ __LINE__ uctx x ty in
          x
      | Some f ->
          let ty, (ret, args, body) =
            handle_letapp uctx
              (ret, (if_rec_function uctx f.x, f.x), f.ty, args, body)
              (Some ty)
          in
          { ty; x = LetApp { ret; f; args; body } })
  | LetDtConstructor { ret; f; args; body }, _ ->
      let fnty = recover_dt_constructor_ty (ret, args) in
      let fty = Prim.get_primitive_under_ty (f, snd fnty) in
      let ty, (ret, args, body) =
        handle_letapp uctx
          (ret, (if_rec_function uctx f, f), fty, args, body)
          (Some ty)
      in
      { ty; x = LetDtConstructor { ret; f; args; body } }
  | LetOp { ret; op; args; body }, _ ->
      let opty =
        Prim.get_primitive_under_ty
          ( Op.op_to_string op,
            NT.construct_arrow_tp (List.map (fun x -> snd x.ty) args, snd ret.ty)
          )
      in
      let fname = Op.op_to_string op in
      let ty, (ret, args, body) =
        handle_letapp uctx
          (ret, (if_rec_function uctx fname, fname), opty, args, body)
          (Some ty)
      in
      { ty; x = LetOp { ret; op; args; body } }
  | LetVal { lhs; rhs; body }, _ ->
      (* let () = *)
      (*   Pp.printf "@{<bold>lhs: %s; rhs: %s;@}\n" (NL.layout_id lhs) *)
      (*     (NL.layout_value rhs) *)
      (* in *)
      handle_letval uctx (lhs, rhs, body) (Some ty)
  | Ite _, _ | Match _, _ ->
      let x = term_type_infer uctx x in
      term_subtyping_check __FILE__ __LINE__ uctx x ty

let type_check (uctx : uctx) x ty =
  (* let ty = Well_found.reduction ty in *)
  (* let ctx = Typectx.empty in *)
  (* let _ = Typectx.close_type ty "v" in *)
  let res = term_type_check uctx x ty in
  res

module SNA = Languages.StrucNA

let struc_check l notations libs r =
  let open SNA in
  (* let () = *)
  (*   List.iter *)
  (*     (fun (name, ty) -> *)
  (*       Pp.printf "@{<bold>%s@}: %s\n" name @@ UT.pretty_layout ty) *)
  (*     libs *)
  (* in *)
  (* let () = failwith "zz" in *)
  let nctx =
    Typectx.(
      List.fold_left
        (fun ctx (name, ty) -> add_to_right ctx (name, ty))
        empty notations)
  in
  let libctx =
    List.fold_left
      (fun ctx (x, ty) -> Nctx.add_to_right ctx (x, ty))
      Nctx.empty libs
  in
  List.iteri
    (fun id (info, (name', ty)) ->
      let id = id + 1 in
      let () = Pp.printf "@{<bold>Task %i:@}\n" id in
      match List.find_opt (fun { name; _ } -> String.equal name name') l with
      | None ->
          _failatwith __FILE__ __LINE__
            (spf "The source code of given refinement type '%s' is missing."
               name')
      | Some { body; _ } ->
          (* let _ = *)
          (*   Dependentcheck.dependent_check *)
          (*     (List.map fst notations @ List.map fst libs) *)
          (*     body *)
          (* in *)
          let () =
            match info with
            | None -> Pp.printf "@{<bold>No Inv:@}\n"
            | Some (id, prop) ->
                Pp.printf "@{<bold>Inv:@} %s = %s\n" id
                  (Autov.pretty_layout_lit prop)
          in
          let () = Pp.printf "@{<bold>TY:@} %s\n" (UT.pretty_layout ty) in
          let ctx = Typectx.empty in
          let rec_info =
            match (info, body.x) with
            | Some (rank_lhs, init_case), NL.(V (Fix (f, _))) ->
                let () =
                  match init_case with
                  | P.(ACint 0) -> ()
                  | lit ->
                      _failatwith __FILE__ __LINE__
                        (spf
                           "unimp: current only support setting the base case \
                            as 0, instead of (%s)."
                           (Autov.pretty_layout_lit lit))
                in
                Some { fix_name = f.x; rank_lhs }
            | None, NL.(V (Fix (fix_name, _))) ->
                failwith
                  (spf "No ranking function for rec function %s" fix_name.x)
            | Some (rank_lhs, _), _ ->
                failwith (spf "Useless ranking function %s" rank_lhs)
            | None, _ -> None
          in
          let res =
            Undersub.type_err_to_false (fun () ->
                type_check { nctx; ctx; libctx; rec_info } body ty)
          in
          if res then
            Pp.printf "@{<bold>@{<yellow>Task %i, type check succeeded@}@}\n" id
          else Pp.printf "@{<bold>@{<red>Task %i, type check failed@}@}\n" id)
    r
