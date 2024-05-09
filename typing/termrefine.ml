open Language
open Rctx
open Checkaux
open Zzdatatype.Datatype
open Sugar
(* open Subtyping *)
(* type t = Nt.t *)

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

and value_type_refine (rctx : rctx) (a : (t, t value) typed) (rty : t rty) :
    t rty option =
  let () = pprint_simple_typectx_judge rctx (layout_typed_value a, rty) in
  let res =
    match (a.x, rty) with
    | _, RtyGhostArr _ -> _failatwith __FILE__ __LINE__ "die"
    | VConst _, _ | VVar _, _ -> (
        let rty' = value_type_infer rctx a in
        match overlap_rty_opt rctx (rty', rty) with
        | Some rty -> Some rty
        | None ->
            _warinning_subtyping_error __FILE__ __LINE__ (rty', rty);
            _warinning_typing_error __FILE__ __LINE__ (layout_typed_value a, rty);
            None)
    | VLam { lamarg; body }, RtyBaseArr { argcty; arg; retty } ->
        let body =
          body #-> (subst_term_instance lamarg.x (VVar arg #: lamarg.ty))
        in
        let argrty = RtyBase { ou = Ex; cty = argcty } in
        let* retty =
          term_type_refine (add_to_right rctx arg #: argrty) body retty
        in
        Some (RtyBaseDepPair { argcty; arg; retty })
    | VLam _, _ -> _failatwith __FILE__ __LINE__ ""
    | VFix _, _ -> _failatwith __FILE__ __LINE__ ""
    | VTu _, _ -> _failatwith __FILE__ __LINE__ ""
  in
  res

and arrow_type_apply (rctx : rctx) appf_rty (apparg : ('t, 't value) typed) =
  (* let () = Printf.printf "appf_rty: %s\n" (layout_rty appf_rty) in *)
  match appf_rty with
  | RtyGhostArr _ -> _failatwith __FILE__ __LINE__ "die"
  | RtyBaseArr { argcty; arg; retty } ->
      (* NOTE: we need to capture the constraint from the argument type *)
      let lit = typed_value_to_typed_lit __FILE__ __LINE__ apparg in
      let retty = subst_rty_instance arg lit.x retty in
      let rctx' =
        match argcty with
        | Cty { phi; _ } ->
            if is_true phi then rctx
            else
              let phi =
                subst_prop_instance default_v
                  (typed_value_to_typed_lit __FILE__ __LINE__ apparg).x phi
              in
              let constraint_rty = prop_to_rty Ex Nt.Ty_unit phi in
              let tmp = (Rename.unique "tmp") #: constraint_rty in
              add_to_right rctx tmp
      in
      Some (rctx', retty)
  | RtyBaseDepPair { argcty; arg; retty } ->
      let apparg_rty = value_type_infer rctx apparg in
      let argrty = cty_to_rty Ex argcty in
      if sub_rty_bool rctx (apparg_rty, argrty) then
        let retty = pack_rty_to_rty (arg #: argrty, retty) in
        let rctx', _ = consume_rty rctx apparg_rty in
        Some (rctx', retty)
      else (
        _warinning_subtyping_error __FILE__ __LINE__ (apparg_rty, argrty);
        _warinning_typing_error __FILE__ __LINE__
          (layout_typed_value apparg, argrty);
        None)
  | RtyArrArr { argrty; retty } ->
      let apparg_rty = value_type_infer rctx apparg in
      if sub_rty_bool rctx (apparg_rty, argrty) then Some (rctx, retty)
      else (
        _warinning_subtyping_error __FILE__ __LINE__ (apparg_rty, argrty);
        _warinning_typing_error __FILE__ __LINE__
          (layout_typed_value apparg, argrty);
        None)
  | _ -> _failatwith __FILE__ __LINE__ "type error: not an arrow type"

and term_type_infer_app (rctx : rctx) (a : ('t, 't term) typed) :
    (rctx * t rty) option =
  let res =
    match a.x with
    | CApp { appf; apparg } ->
        let appf_rty = value_type_infer rctx appf in
        let* rctx', retty = arrow_type_apply rctx appf_rty apparg in
        Some (rctx', retty)
    | CAppOp { op; appopargs } ->
        let op_rty =
          _id_type_infer __FILE__ __LINE__ rctx (op_name_for_typectx op.x)
        in
        let* rctx, retty =
          List.fold_left
            (fun res apparg ->
              let* rctx, op_rty = res in
              let* rctx, op_rty = arrow_type_apply rctx op_rty apparg in
              Some (rctx, op_rty))
            (Some (rctx, op_rty))
            appopargs
        in
        Some (rctx, retty)
    | _ ->
        let* rty = term_type_infer rctx a in
        Some (rctx, rty)
  in
  res

and term_type_infer (rctx : rctx) (a : ('t, 't term) typed) : t rty option =
  let rty =
    match a.x with
    | CErr -> _failatwith __FILE__ __LINE__ "die"
    | CVal v -> Some (value_type_infer rctx v)
    | CApp _ | CAppOp _ -> _failatwith __FILE__ __LINE__ "die"
    | CLetE { rhs; lhs; body } ->
        let* rctx', rty' = term_type_infer_app rctx rhs in
        let lhs = lhs.x #: rty' in
        let* rty = term_type_infer (add_to_right rctx' lhs) body in
        let rty = pack_rty_to_rty (lhs, rty) in
        (* let _ = *)
        (*   Printf.printf "CLetE exists %s\n" *)
        (*   @@ List.split_by_comma (fun x -> x.x) bindings *)
        (* in *)
        Some rty
    | CMatch _ -> _failatwith __FILE__ __LINE__ "die"
    | CLetDeTu _ -> failwith "unimp"
  in
  let () =
    match rty with
    | Some rty -> pprint_simple_typectx_infer rctx (layout_typed_term a, rty)
    | None -> ()
  in
  rty

and match_case_type_refine (rctx : rctx) (matched : (t, t value) typed)
    (x : t match_case) (rty : t rty) : t rty option =
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
      let* rty = term_type_refine (add_to_rights rctx' bindings) exp rty in
      (* let args' = diff_ctx (rctx, rctx') in *)
      let rty = pack_rtys_to_rty bindings rty in
      (* let _ = *)
      (*   Printf.printf "exists %s\n" *)
      (*   @@ List.split_by_comma (fun x -> x.x) bindings *)
      (* in *)
      Some rty

and term_type_refine (rctx : rctx) (y : ('t, 't term) typed) (rty : t rty) :
    t rty option =
  let () = pprint_simple_typectx_judge rctx (layout_typed_term y, rty) in
  match y.x with
  | CLetDeTu _ -> failwith "unimp"
  | CVal v -> value_type_refine rctx v rty
  | CErr -> _failatwith __FILE__ __LINE__ "unimp"
  | CApp _ | CAppOp _ -> (
      let* rctx', rty' = term_type_infer_app rctx y in
      match overlap_rty_opt rctx' (rty', rty) with
      | Some rty -> Some rty
      | None ->
          _warinning_subtyping_error __FILE__ __LINE__ (rty', rty);
          _warinning_typing_error __FILE__ __LINE__ (layout_typed_term y, rty);
          None)
  | CMatch { matched; match_cases } ->
      (* NOTE: we drop unreachable cases *)
      let match_cases =
        List.filter_map
          (fun case -> match_case_type_refine rctx matched case rty)
          match_cases
      in
      if List.length match_cases == 0 then (
        _warinning_typing_error __FILE__ __LINE__ (layout_typed_term y, rty);
        None)
      else Some (intersect_rtys match_cases)
  | CLetE { rhs; lhs; body } ->
      let* rctx', rty' = term_type_infer_app rctx rhs in
      let lhs = lhs.x #: rty' in
      let* rty = term_type_refine (add_to_right rctx' lhs) body rty in
      let rty = pack_rty_to_rty (lhs, rty) in
      (* let _ = *)
      (*   Printf.printf "CLetE exists %s\n" *)
      (*   @@ List.split_by_comma (fun x -> x.x) bindings *)
      (* in *)
      Some rty
