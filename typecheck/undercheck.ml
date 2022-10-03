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

(* TODO: a real reachability check *)
let reachability_check _ ty =
  let res =
    match UT.assume_base_destruct_opt ty with
    | Some (_, _, prop) -> (
        P.(match peval prop with Lit (ACbool false) -> false | _ -> true))
    | _ -> true
  in
  (* let () = *)
  (*   Pp.printf "@{<bold>reachability_check@} %s: %b\n" (UT.pretty_layout ty) res *)
  (* in *)
  res

let rec value_type_infer (uctx : uctx) (a : NL.value NL.typed) :
    UL.value UL.typed =
  let aty = a.ty in
  let open UL in
  match a.x with
  | NL.Exn -> { ty = UT.nt_to_exn_type (snd aty); x = Exn }
  | NL.Lit lit ->
      typed_map (fun x -> Lit x) @@ lit_type_infer uctx { ty = aty; x = lit }
  | NL.Lam (id, body) -> (
      let typename =
        match fst id.ty with
        | None -> _failatwith __FILE__ __LINE__ "die"
        | Some x -> x
      in
      let idty =
        match Nctx.get_opt uctx.nctx typename with
        | Some (_, idty) -> idty
        | None ->
            _failatwith __FILE__ __LINE__
            @@ spf "cannot find the notation of %s (named %s)" id.x typename
      in
      let id = erase_check_mk_id __FILE__ __LINE__ id idty in
      (* let body = *)
      (*   term_type_infer *)
      (*     { uctx with ctx = Typectx.force_add_to_right uctx.ctx id } *)
      (*     body *)
      (* in *)
      (* let ty = *)
      (*   UT.UnderTy_arrow { argname = id.x; argty = id.ty; retty = body.ty } *)
      (* in *)
      (* { ty; x = Lam (id, body) } *)
      match Typectx.add_to_right reachability_check uctx.ctx id with
      | None ->
          let body = term_set_bot body in
          { ty = body.ty; x = Lam (id, body) }
      | Some ctx ->
          let body = term_type_infer { uctx with ctx } body in
          let ty =
            UT.UnderTy_arrow { argname = id.x; argty = id.ty; retty = body.ty }
          in
          { ty; x = Lam (id, body) })
  | NL.Fix _ -> _failatwith __FILE__ __LINE__ "unimp"

and value_type_check (uctx : uctx) (a : NL.value NL.typed) (ty : UT.t) :
    UL.value UL.typed =
  let open UT in
  let result =
    match (a.NL.x, ty) with
    | NL.Exn, _ ->
        let x = value_type_infer uctx a in
        term_subtyping_check __FILE__ __LINE__ uctx.ctx x ty
    | NL.Lit _, _ ->
        let x = value_type_infer uctx a in
        term_subtyping_check __FILE__ __LINE__ uctx.ctx x ty
    | NL.Lam (id, body), UnderTy_arrow { argname; argty; retty } -> (
        let () =
          match Nctx.get_opt uctx.nctx id.x with
          | Some _ -> _failatwith __FILE__ __LINE__ "die"
          | None -> ()
        in
        let id = erase_check_mk_id __FILE__ __LINE__ id argty in
        let retty = UT.subst_id retty argname id.x in
        (* let body = term_type_check { uctx with ctx = Typectx.force_add_to_right uctx.ctx id } body retty in *)
        (* { *)
        (*   ty = UnderTy_arrow { argname; argty; retty = body.ty }; *)
        (*   x = Lam (id, body); *)
        (* } *)
        match Typectx.add_to_right reachability_check uctx.ctx id with
        | None ->
            let body = term_set_bot body in
            let () = subtyping_check __FILE__ __LINE__ uctx.ctx body.ty retty in
            { ty = retty; x = Lam (id, body) }
        | Some ctx ->
            let body = term_type_check { uctx with ctx } body retty in
            {
              ty = UnderTy_arrow { argname; argty; retty = body.ty };
              x = Lam (id, body);
            })
    | NL.Fix (f, _), ty ->
        let _ = erase_check_mk_id __FILE__ __LINE__ f ty in
        _failatwith __FILE__ __LINE__ "unimp"
        (* let body = *)
        (*   value_type_check *)
        (*     { uctx with ctx = Typectx.add_to_right uctx.ctx f } *)
        (*     body ty *)
        (* in *)
        (* { ty = body.ty; x = Fix (f, body) } *)
    | _, _ -> _failatwith __FILE__ __LINE__ ""
  in
  result

and handle_let_body (uctx : uctx) ctx' body target_type =
  let trivial_res =
    let body = term_set_bot body in
    (body.ty, body)
  in
  match (ctx', target_type) with
  | None, None -> trivial_res
  | Some ctx', None ->
      let body = term_type_infer { uctx with ctx = ctx' } body in
      (Typectx.close_by_diff ctx' uctx.ctx body.ty, body)
  | None, Some ty ->
      let ty', _ = trivial_res in
      let () = subtyping_check __FILE__ __LINE__ uctx.ctx ty' ty in
      trivial_res
  | Some ctx', Some ty ->
      let body = term_type_check { uctx with ctx = ctx' } body ty in
      (ty, body)

and handle_lettu (uctx : uctx) (tu, args, body) target_type =
  let open UL in
  let args = List.map (id_type_infer uctx) args in
  let tu =
    erase_check_mk_id __FILE__ __LINE__ tu
      (UT.UnderTy_tuple
         (List.map
            (fun x ->
              let name = Rename.unique x.x in
              (name, x.ty))
            args))
  in
  let ctx' = Typectx.add_to_right reachability_check uctx.ctx tu in
  let ty, body = handle_let_body uctx ctx' body target_type in
  UL.{ ty; x = LetTu { tu; args; body } }

and handle_letdetu (uctx : uctx) (tu, args, body) target_type =
  let open UL in
  let tu = id_type_infer uctx tu in
  let rec loop = function
    | [], [] -> []
    | x :: args, (name, ty) :: ts ->
        let ts =
          List.map (fun (a, ty) -> (a, UT.subst_id ty name x.NNtyped.x)) ts
        in
        erase_check_mk_id __FILE__ __LINE__ x ty :: loop (args, ts)
    | _ -> _failatwith __FILE__ __LINE__ ""
  in
  let args = loop (args, UT.assume_tuple __FILE__ __LINE__ tu.ty) in
  let ctx' = Typectx.add_to_rights reachability_check uctx.ctx args in
  let ty, body = handle_let_body uctx ctx' body target_type in
  UL.{ ty; x = LetDeTu { tu; args; body } }

and handle_letapp (uctx : uctx) (ret, fty, args, body) target_type =
  let open UL in
  let open UT in
  let args = List.map (id_type_infer uctx) args in
  let () = Typectx.pretty_print_app_judge uctx.ctx (args, fty) in
  (* arguments type check *)
  let rec aux = function
    | [], ty -> ty
    | arg :: args, UnderTy_poly_arrow { argname; retty; _ } ->
        (* let () = erase_check __FILE__ __LINE__ (arg.ty, (arg.x, argnty)) in *)
        let retty = subst_id retty argname arg.x in
        aux (args, retty)
    | arg :: args, UnderTy_arrow { argname; retty; argty } ->
        let () = subtyping_check __FILE__ __LINE__ uctx.ctx arg.ty argty in
        let retty = subst_id retty argname arg.x in
        aux (args, retty)
    | _, _ -> _failatwith __FILE__ __LINE__ ""
  in
  (* let () = Pp.printf "@{<bold>Begin@}\n" in *)
  let trivial_res =
    let retty = make_basic_bot (snd ret.NL.ty) in
    let ret = erase_check_mk_id __FILE__ __LINE__ ret retty in
    (retty, (ret, args, term_set_bot body))
  in
  let ret =
    try
      let retty = aux (args, fty) in
      let ret = erase_check_mk_id __FILE__ __LINE__ ret retty in
      Some ret
    with
    | Autov.FailWithModel (msg, _) ->
        let () = Pp.printf "@{<orange>1Application failed:@}%s\n" msg in
        None
    | Autov.SMTTIMEOUT ->
        let () = Pp.printf "@{<orange>Application failed:@}%s\n" "timeout" in
        None
    | e -> raise e
  in
  match ret with
  | None -> trivial_res
  | Some ret ->
      let ctx' = Typectx.add_to_right reachability_check uctx.ctx ret in
      let ty, body = handle_let_body uctx ctx' body target_type in
      (ty, (ret, args, body))

and handle_letval (uctx : uctx) (lhs, rhs, body) target_type =
  let open UL in
  let rhs = value_type_infer uctx rhs in
  let lhs = erase_check_mk_id __FILE__ __LINE__ lhs rhs.ty in
  let ctx' = Typectx.add_to_right reachability_check uctx.ctx lhs in
  let ty, body = handle_let_body uctx ctx' body target_type in
  UL.{ ty; x = LetVal { lhs; rhs; body } }

and handle_ite uctx (cond, e_t, e_f) =
  let open UL in
  (* let () = *)
  (*   Pp.printf "@{<bold>Before If@}\n"; *)
  (*   Typectx.pretty_print uctx.ctx *)
  (* in *)
  let cond = id_type_infer uctx cond in
  let handle_case ty e =
    let ctx' =
      Typectx.update uctx.ctx (cond.x, fun ty' -> UT.conjunct ty ty')
    in
    let e = term_type_infer { uctx with ctx = ctx' } e in
    let ty = UT.add_ex_uprop false cond.x ty e.ty in
    { x = e.x; ty }
  in
  let e_t =
    handle_case (UT.make_basic_from_prop NT.Ty_bool P.bvar_to_prop) e_t
  in
  let e_f =
    handle_case
      (UT.make_basic_from_prop NT.Ty_bool (fun x -> P.(Not (bvar_to_prop x))))
      e_f
  in
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
      | UnderTy_arrow { argname; argty; retty = UnderTy_tuple ts } ->
          let ts =
            List.map
              (fun (name, t) -> (name, UT.subst_id t argname matched.x))
              ts
          in
          let rec loop = function
            | [], [] -> []
            | arg :: args, (name, ty) :: ts ->
                { x = arg; ty }
                :: loop
                     ( args,
                       List.map (fun (a, ty) -> (a, UT.subst_id ty name arg)) ts
                     )
            | _, _ -> _failatwith __FILE__ __LINE__ ""
          in
          (argty, loop (args, ts))
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
    try
      let () = subtyping_check __FILE__ __LINE__ uctx.ctx matched.ty retty in
      let ctx' = Typectx.update uctx.ctx (matched.x, fun _ -> retty) in
      let ctx'' = Typectx.force_add_to_rights ctx' args in
      let exp = term_type_infer { uctx with ctx = ctx'' } exp in
      ( Typectx.close_by_diff ctx'' ctx' exp.ty,
        UL.{ constructor; args = List.map (fun x -> x.x) args; exp } )
    with
    | Autov.FailWithModel (msg, _) ->
        let () = Pp.printf "@{<orange>Match failed:@}%s\n" msg in
        trivial_res
    | Autov.SMTTIMEOUT ->
        let () = Pp.printf "@{<orange>Match failed:@}%s\n" "timeout" in
        trivial_res
    | e -> raise e
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
        let ty, (ret, args, body) =
          handle_letapp uctx (ret, opty, args, body) None
        in
        { ty; x = LetOp { ret; op; args; body } }
    | LetApp { ret; f; args; body } ->
        let f = id_type_infer uctx f in
        let ty, (ret, args, body) =
          handle_letapp uctx (ret, f.ty, args, body) None
        in
        { ty; x = LetApp { ret; f; args; body } }
    | LetDtConstructor { ret; f; args; body } ->
        let fnty = recover_dt_constructor_ty (ret, args) in
        let fty = Prim.get_primitive_under_ty (f, snd fnty) in
        let ty, (ret, args, body) =
          handle_letapp uctx (ret, fty, args, body) None
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
  | LetTu { tu; args; body }, _ -> handle_lettu uctx (tu, args, body) (Some ty)
  | LetDeTu { tu; args; body }, _ ->
      handle_letdetu uctx (tu, args, body) (Some ty)
  | LetApp { ret; f; args; body }, _ ->
      let f = id_type_infer uctx f in
      let ty, (ret, args, body) =
        handle_letapp uctx (ret, f.ty, args, body) (Some ty)
      in
      { ty; x = LetApp { ret; f; args; body } }
  | LetDtConstructor { ret; f; args; body }, _ ->
      let fnty = recover_dt_constructor_ty (ret, args) in
      let fty = Prim.get_primitive_under_ty (f, snd fnty) in
      let ty, (ret, args, body) =
        handle_letapp uctx (ret, fty, args, body) (Some ty)
      in
      { ty; x = LetDtConstructor { ret; f; args; body } }
  | LetOp { ret; op; args; body }, _ ->
      let opty =
        Prim.get_primitive_under_ty
          ( Op.op_to_string op,
            NT.construct_arrow_tp (List.map (fun x -> snd x.ty) args, snd ret.ty)
          )
      in
      let ty, (ret, args, body) =
        handle_letapp uctx (ret, opty, args, body) (Some ty)
      in
      { ty; x = LetOp { ret; op; args; body } }
  | LetVal { lhs; rhs; body }, _ ->
      let () =
        Pp.printf "@{<bold>lhs: %s; rhs: %s;@}\n" (NL.layout_id lhs)
          (NL.layout_value rhs)
      in
      handle_letval uctx (lhs, rhs, body) (Some ty)
  | Ite _, _ | Match _, _ ->
      let x = term_type_infer uctx x in
      let () = subtyping_check __FILE__ __LINE__ uctx.ctx x.ty ty in
      (* NOTE: underappproximate here *)
      { ty; x = x.x }

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
  List.iteri
    (fun id (name', ty) ->
      let id = id + 1 in
      let () = Pp.printf "@{<bold>Task %i:@}\n" id in
      match List.find_opt (fun { name; _ } -> String.equal name name') l with
      | None ->
          _failatwith __FILE__ __LINE__
            (spf "The source code of given refinement type '%s' is missing."
               name')
      | Some { body; _ } -> (
          let _ =
            Dependentcheck.dependent_check
              (List.map fst notations @ List.map fst libs)
              body
          in
          let () = Pp.printf "@{<bold>TY:@} %s\n" (UT.pretty_layout ty) in
          let nctx =
            Nctx.(
              List.fold_left
                (fun ctx (name, ty) -> add_to_right ctx (name, ty))
                empty notations)
          in
          let libctx =
            List.fold_left
              (fun ctx (x, ty) -> Typectx.force_add_to_right ctx { x; ty })
              Typectx.empty libs
          in
          let ctx = Typectx.empty in
          try
            let _ = type_check { nctx; ctx; libctx } body ty in
            let _ =
              Pp.printf "@{<bold>@{<yellow>Task %i, type check succeeded@}@}\n"
                id
            in
            ()
          with Autov.FailWithModel _ ->
            Pp.printf "@{<bold>@{<red>Task %i, type check failed@}@}\n" id))
    r
