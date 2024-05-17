open Language
open Rctx
open Checkaux
open Zzdatatype.Datatype
open Sugar
(* open Subtyping *)
(* type t = Nt.t *)

let ghost_recurisve_rty_unfold (argcty, arg, retty) =
  (* NOTE: We assume the index type is nature number *)
  let argnty = erase_cty argcty in
  let natural_num_constraint =
    List.fold_left apply_pi_prop
      (Env.get_statements_by_name "natrual_number")
      [ mk_typed_lit_by_id default_v #: argnty ]
  in
  let argcty = map_phi_in_cty (smart_add_to natural_num_constraint) argcty in
  let index = arg #: (mk_rty Fa @@ argcty) in
  let self_rty =
    (* let arg' = Rename.unique arg in *)
    (* let retty' = subst_rty_instance arg (AVar arg' #: argnty) retty in *)
    let phi' =
      List.fold_left apply_pi_prop
        (Env.get_statements_by_name "rec_arg")
        [
          mk_typed_lit_by_id arg #: argnty;
          mk_typed_lit_by_id default_v #: argnty;
        ]
    in
    let argcty' = map_cty_on_phi argcty (smart_add_to phi') in
    let rty = RtyGhostArr { argcty = argcty'; arg; retty } in
    rty
  in
  (index, self_rty)

let rec value_type_infer (rctx : rctx) (a : (t, t value) typed) : t rty =
  let rty =
    match a.x with
    | VVar id -> (
        let res = _id_type_infer __FILE__ __LINE__ rctx id.x in
        match erase_rty res with
        | Nt.Ty_arrow _ -> res
        | _ -> mk_rty_var_eq_var Ex a.ty (default_v, id.x))
    | VConst c -> const_type_infer a.ty c
    | VLam _ | VFix _ | VTu _ -> _failatwith __FILE__ __LINE__ "unimp"
  in
  let rty = alpha_renaming_rty_value rctx a rty in
  let () = pprint_simple_typectx_infer rctx (layout_typed_value a, rty) in
  rty

and value_type_check (rctx : rctx) (a : (t, t value) typed) (rty : t rty) :
    unit option =
  let () = pprint_simple_typectx_judge rctx (layout_typed_value a, rty) in
  let res =
    match (a.x, rty) with
    | _, RtyIntersect _ -> (
        let rtys = rty_intersect_to_rtys rty in
        match a.x with
        | VFix { fixname; fixarg; body } ->
            let* _ =
              List.fold_left
                (fun checked_rtys rty ->
                  let* checked_rtys = checked_rtys in
                  let () =
                    Pp.printf
                      "Now type check @{<yellow>%s@} from the intersection type\n"
                      (layout_rty rty)
                  in
                  match rty with
                  | RtyGhostArr { argcty; arg; retty } ->
                      let index, self_rty =
                        ghost_recurisve_rty_unfold (argcty, arg, retty)
                      in
                      let rctx = add_to_left rctx index in
                      let binding =
                        [
                          fixname.x
                          #: (rty_mk_intersect (checked_rtys @ [ self_rty ]));
                        ]
                      in
                      let rctx' = add_to_rights rctx binding in
                      let* _ =
                        value_type_check rctx'
                          (VLam { lamarg = fixarg; body }) #: fixname.ty
                          retty
                      in
                      Some (checked_rtys @ [ rty ])
                  | _ -> _failatwith __FILE__ __LINE__ "die")
                (Some []) rtys
            in
            Some ()
        | _ ->
            List.fold_left
              (fun result rty ->
                let* _ = result in
                let () =
                  Pp.printf
                    "Now type check @{<yellow>%s@} from the intersection type\n"
                    (layout_rty rty)
                in
                value_type_check rctx a rty)
              (Some ()) rtys)
    | VConst _, _ | VVar _, _ ->
        let rty' = value_type_infer rctx a in
        if sub_rty_bool rctx (rty', rty) then Some ()
        else (
          _warinning_subtyping_error __FILE__ __LINE__ (rty', rty);
          _warinning_typing_error __FILE__ __LINE__ (layout_typed_value a, rty);
          None)
    | VLam { lamarg; body }, RtyBaseArr { argcty; arg; retty } ->
        let body =
          body #-> (subst_term_instance lamarg.x (VVar arg #: lamarg.ty))
        in
        let argrty = RtyBase { ou = Fa; cty = argcty } in
        term_type_check (add_to_right rctx arg #: argrty) body retty
    | VLam { lamarg; body }, RtyBaseDepPair { argcty; arg; retty } ->
        let body =
          body #-> (subst_term_instance lamarg.x (VVar arg #: lamarg.ty))
        in
        let argrty = RtyBase { ou = Ex; cty = argcty } in
        term_type_check (add_to_right rctx arg #: argrty) body retty
    | VLam { lamarg; body }, RtyArrArr { argrty; retty } ->
        term_type_check (add_to_right rctx lamarg.x #: argrty) body retty
    | VLam _, RtyGhostArr { argcty; arg; retty } ->
        let index = arg #: (mk_rty Fa @@ argcty) in
        let rctx = add_to_right rctx index in
        value_type_check rctx a retty
    | VLam _, _ -> _failatwith __FILE__ __LINE__ ""
    | VFix { fixname; fixarg; body }, RtyBaseArr { argcty; arg; retty } ->
        let rec_constraint_cty = apply_rec_arg arg #: fixarg.ty in
        let rty' =
          let a = { x = Rename.unique arg; ty = fixarg.ty } in
          RtyBaseArr
            {
              argcty = intersect_ctys [ argcty; rec_constraint_cty ];
              arg = a.x;
              retty = subst_rty_instance arg (AVar a) retty;
            }
        in
        let binding = arg #: (RtyBase { ou = Fa; cty = argcty }) in
        let body =
          body #-> (subst_term_instance fixarg.x (VVar arg #: fixarg.ty))
        in
        term_type_check
          (add_to_rights rctx [ binding; fixname.x #: rty' ])
          body retty
    | VFix { fixname; fixarg; body }, RtyGhostArr { argcty; arg; retty }
      when Nt.eq (erase_cty argcty) Nt.Ty_int ->
        let index, self_rty = ghost_recurisve_rty_unfold (argcty, arg, retty) in
        let rctx = add_to_left rctx index in
        let binding = [ fixname.x #: self_rty ] in
        (* let () = Printf.printf "%s\n" index'.x in *)
        (* let () = Printf.printf "%s\n" fixname.x in *)
        (* let () = Printf.printf "%s\n" arg in *)
        value_type_check
          (add_to_rights rctx binding)
          (VLam { lamarg = fixarg; body }) #: fixname.ty
          retty
    | VFix _, _ -> _failatwith __FILE__ __LINE__ ""
    | VTu _, _ -> _failatwith __FILE__ __LINE__ ""
  in
  res

and match_case_type_check (rctx : rctx) (matched : (t, t value) typed)
    (x : t match_case) (rty : t rty) : unit option =
  (* let rctx', matched_rty = consume_rty rctx (value_type_infer rctx matched) in *)
  (* let matched_cty = rty_to_cty (value_type_infer rctx matched) in *)
  match x with
  | CMatchcase { constructor; args; exp } ->
      let constructor_rty =
        _id_type_infer __FILE__ __LINE__ rctx
          (dt_name_for_typectx constructor.x)
      in
      let args, retty =
        List.fold_left
          (fun (args, rty) x ->
            match rty with
            | RtyBaseArr { argcty; arg; retty } ->
                let retty = subst_rty_instance arg (AVar x) retty in
                let x = x.x #: (RtyBase { ou = Ex; cty = argcty }) in
                (args @ [ x ], retty)
            | RtyArrArr { argrty; retty } ->
                let x = x.x #: argrty in
                (args @ [ x ], retty)
            | _ -> _failatwith __FILE__ __LINE__ "die")
          ([], constructor_rty) args
      in
      let retcty = rty_to_cty retty in
      let rctx', retty =
        match args with
        | [] -> (
            match retcty with
            | Cty { phi; _ } ->
                let lit = typed_value_to_typed_lit __FILE__ __LINE__ matched in
                let cty =
                  Cty
                    {
                      nty = Ty_unit;
                      phi = subst_prop_instance default_v lit.x phi;
                    }
                in
                (rctx, RtyBase { cty; ou = Ex }))
        | _ ->
            let rctx', matched_rty =
              consume_rty rctx (value_type_infer rctx matched)
            in
            let matched_cty = rty_to_cty matched_rty in
            let cty = intersect_ctys [ matched_cty; retcty ] in
            (rctx', RtyBase { cty; ou = Ex })
      in
      let bindings = args @ [ (Rename.unique "tmp") #: retty ] in
      let* _ = term_type_check (add_to_rights rctx' bindings) exp rty in
      (* let _ = *)
      (*   Printf.printf "exists %s\n" *)
      (*   @@ List.split_by_comma (fun x -> x.x) bindings *)
      (* in *)
      Some ()

and match_case_type_infer (rctx : rctx) (matched : (t, t value) typed)
    (x : t match_case) : t rty option =
  match x with
  | CMatchcase { constructor; args; exp } ->
      let constructor_rty =
        _id_type_infer __FILE__ __LINE__ rctx
          (dt_name_for_typectx constructor.x)
      in
      let args, retty =
        List.fold_left
          (fun (args, rty) x ->
            match rty with
            | RtyBaseArr { argcty; arg; retty } ->
                let retty = subst_rty_instance arg (AVar x) retty in
                let x = x.x #: (RtyBase { ou = Ex; cty = argcty }) in
                (args @ [ x ], retty)
            | RtyArrArr { argrty; retty } ->
                let x = x.x #: argrty in
                (args @ [ x ], retty)
            | _ -> _failatwith __FILE__ __LINE__ "die")
          ([], constructor_rty) args
      in
      (* let retcty, _ = *)
      (*   match retty with *)
      (*   | RtyBase { cty; er; ou = Ex } -> (cty, er) *)
      (*   | _ -> _failatwith __FILE__ __LINE__ "die" *)
      (* in *)
      (* let () = Printf.printf "retcty: %s\n" (layout_cty retcty) in *)
      let rctx', retcty =
        let lit = typed_value_to_typed_lit __FILE__ __LINE__ matched in
        let phi = get_cty_prop (rty_to_cty retty) in
        let cty' =
          Cty { nty = Ty_unit; phi = subst_prop_instance default_v lit.x phi }
        in
        let used_in_this_case =
          List.length
            (List.interset
               (fun a b -> String.equal a.x b.x)
               args (fv_term exp.x))
          > 0
        in
        let rctx' =
          if used_in_this_case then
            fst @@ consume_rty rctx (value_type_infer rctx matched)
          else rctx
        in
        (rctx', cty')
      in
      let forward_ctx =
        add_to_rights rctx'
          (args @ [ (Rename.unique "tmp") #: (mk_rty Fa retcty) ])
      in
      let* rty = term_type_infer forward_ctx exp in
      let rty =
        pack_rtys_to_rty
          (args @ [ (Rename.unique "tmp") #: (mk_rty Ex retcty) ])
          rty
      in
      let _ =
        let code = (CMatch { matched; match_cases = [ x ] }) #: exp.ty in
        let _ = Pp.printf "@{<bold>Match Case Infer:@}\n" in
        pprint_simple_typectx_infer rctx (layout_typed_term code, rty)
      in
      Some rty

and arrow_type_apply (rctx : rctx) appf_rty (apparg : ('t, 't value) typed) =
  let () =
    Pp.printf "applying type @{<yellow>%s@} on argument @{<orange>%s@}\n"
      (layout_rty appf_rty)
      (layout_typed_value apparg)
  in
  match appf_rty with
  | RtyGhostArr { argcty; arg; retty } ->
      let argnty = erase_cty argcty in
      let arg', appf_rty' = alpha_renaming arg #: argnty retty in
      (* NOTE: still, forward using overapproximation, backward using underapproximation. *)
      let rctx' = add_to_right rctx arg'.x #: (cty_to_rty Fa argcty) in
      let* forward_rctx, backward_binding, retty =
        arrow_type_apply rctx' appf_rty' apparg
      in
      Some
        ( forward_rctx,
          (arg'.x #: (cty_to_rty Ex argcty)) :: backward_binding,
          retty )
  | RtyBaseArr { argcty; arg; retty } ->
      (* NOTE: we need to capture the constraint from the argument type *)
      let lit = typed_value_to_typed_lit __FILE__ __LINE__ apparg in
      let retty = subst_rty_instance arg lit.x retty in
      let phi =
        subst_prop_instance default_v
          (typed_value_to_typed_lit __FILE__ __LINE__ apparg).x
        @@ get_cty_prop argcty
      in
      if is_true phi then Some (rctx, [], retty)
      else
        let forward_rctx =
          add_to_right rctx
            (Rename.unique "tmp") #: (prop_to_rty Fa Nt.Ty_unit phi)
        in
        let backward_binding =
          [ (Rename.unique "tmp") #: (prop_to_rty Ex Nt.Ty_unit phi) ]
        in
        Some (forward_rctx, backward_binding, retty)
  | RtyBaseDepPair { argcty; arg; retty } ->
      let apparg_rty = value_type_infer rctx apparg in
      let argrty = cty_to_rty Ex argcty in
      if sub_rty_bool rctx (apparg_rty, argrty) then
        let rctx', _ = consume_rty rctx apparg_rty in
        let retty =
          subst_rty_instance arg
            (typed_value_to_typed_lit __FILE__ __LINE__ apparg).x retty
        in
        let phi =
          subst_prop_instance default_v
            (typed_value_to_typed_lit __FILE__ __LINE__ apparg).x
          @@ get_cty_prop argcty
        in
        (* let () = Printf.printf "phi: %s\n" (layout_prop phi) in *)
        let forward_rctx =
          add_to_right rctx'
            (Rename.unique "tmp") #: (prop_to_rty Fa Nt.Ty_unit phi)
        in
        let backward_rctx =
          [ (Rename.unique "tmp") #: (prop_to_rty Ex Nt.Ty_unit phi) ]
        in
        Some (forward_rctx, backward_rctx, retty)
      else (
        _warinning_subtyping_error __FILE__ __LINE__ (apparg_rty, argrty);
        _warinning_typing_error __FILE__ __LINE__
          (layout_typed_value apparg, argrty);
        None)
  | RtyArrArr { argrty; retty } ->
      let apparg_rty = value_type_infer rctx apparg in
      if sub_rty_bool rctx (apparg_rty, argrty) then Some (rctx, [], retty)
      else (
        _warinning_subtyping_error __FILE__ __LINE__ (apparg_rty, argrty);
        _warinning_typing_error __FILE__ __LINE__
          (layout_typed_value apparg, argrty);
        None)
  | RtyIntersect _ ->
      let appf_rtys = rty_intersect_to_rtys appf_rty in
      (* NOTE: we choose the first valid application *)
      let rec aux appf_rtys =
        match appf_rtys with
        | [] -> None
        | appf_rty :: appf_rtys -> (
            let res = arrow_type_apply rctx appf_rty apparg in
            match res with
            | None -> aux appf_rtys
            | Some _ ->
                let () =
                  Pp.printf
                    "We choose type @{<yellow>%s@} from the intersection type\n"
                    (layout_rty appf_rty)
                in
                res)
      in
      aux appf_rtys
  | _ -> _failatwith __FILE__ __LINE__ "type error: not an arrow type"

and term_type_infer_app (rctx : rctx) (a : ('t, 't term) typed) :
    (rctx * (t rty, string) typed list * t rty) option =
  let res =
    match a.x with
    | CApp { appf; apparg } ->
        let appf_rty = value_type_infer rctx appf in
        let* forward_rctx, backward_bindings, retty =
          arrow_type_apply rctx appf_rty apparg
        in
        let _ =
          Printf.printf "arrow_type_apply bindings: %s\n"
            (layout_bindings backward_bindings)
        in
        Some (forward_rctx, backward_bindings, retty)
    | CAppOp { op; appopargs } ->
        let op_rty =
          _id_type_infer __FILE__ __LINE__ rctx (op_name_for_typectx op.x)
        in
        let* rctx, backward_bindings, retty =
          List.fold_left
            (fun res apparg ->
              let* rctx, backward_bindings, op_rty = res in
              let* rctx, backward_bindings', op_rty =
                arrow_type_apply rctx op_rty apparg
              in
              Some (rctx, backward_bindings @ backward_bindings', op_rty))
            (Some (rctx, [], op_rty))
            appopargs
        in
        Some (rctx, backward_bindings, retty)
    | _ ->
        let* rty = term_type_infer rctx a in
        Some (rctx, [], rty)
  in
  res

and term_type_infer (rctx : rctx) (y : ('t, 't term) typed) : t rty option =
  let rty =
    match y.x with
    | CErr ->
        (* NOTE: Our language doesn't consider execptions, however, this can help us focus on part of code during debuging. *)
        Some (mk_rty_false Ex y.ty)
    | CVal v -> Some (value_type_infer rctx v)
    | CApp _ | CAppOp _ ->
        let* _, bindings, rty' = term_type_infer_app rctx y in
        let _ = Printf.printf "bindings: %s\n" (layout_bindings bindings) in
        Some (pack_rtys_to_rty bindings rty')
    | CLetE { rhs; lhs; body } ->
        let* rctx', bindings, rty' = term_type_infer_app rctx rhs in
        let _ =
          Printf.printf "term_type_infer_app bindings: %s\n"
            (layout_bindings bindings)
        in
        let lhs = lhs.x #: rty' in
        let* rty' = term_type_infer (add_to_right rctx' lhs) body in
        let _ =
          Printf.printf "bindings: %s\n" (layout_bindings (bindings @ [ lhs ]))
        in
        Some (pack_rtys_to_rty (bindings @ [ lhs ]) rty')
    | CMatch { matched; match_cases } -> (
        let rtys =
          List.filter_map
            (fun case -> match_case_type_infer rctx matched case)
            match_cases
        in
        match rtys with [] -> None | _ -> Some (intersect_rtys rtys))
    | CLetDeTu _ -> failwith "unimp"
  in
  let () =
    match rty with
    | Some rty -> pprint_simple_typectx_infer rctx (layout_typed_term y, rty)
    | None -> ()
  in
  rty

and term_type_check (rctx : rctx) (y : ('t, 't term) typed) (rty : t rty) :
    unit option =
  let () =
    match y.x with
    | CVal _ -> ()
    | _ -> pprint_simple_typectx_judge rctx (layout_typed_term y, rty)
  in
  match y.x with
  | CLetDeTu _ -> failwith "unimp"
  | CVal v -> value_type_check rctx v rty
  | CErr -> _failatwith __FILE__ __LINE__ "unimp"
  | _ ->
      let* rty' = term_type_infer rctx y in
      if sub_rty_bool rctx (rty', rty) then Some ()
      else (
        _warinning_subtyping_error __FILE__ __LINE__ (rty', rty);
        _warinning_typing_error __FILE__ __LINE__ (layout_typed_term y, rty);
        None)
