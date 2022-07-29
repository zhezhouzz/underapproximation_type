module NL = Languages.NormalAnormal
module UL = Languages.UnderAnormal
module NT = Languages.Normalty
module UT = Languages.Underty
open Zzdatatype.Datatype
open Abstraction
open Sugar

let layout_judge = Frontend.Typectx.pretty_layout_under_judge Trans.nan_to_term

let erase_check file line (underfty, normalty) =
  (* let () = *)
  (*   Printf.printf "|_ %s _| = %s\n" *)
  (*     (Frontend.Undertype.layout underfty) *)
  (*     (Frontend.Type.layout @@ UT.erase underfty) *)
  (* in *)
  let _ = _check_equality file line NT.eq (UT.erase underfty) normalty in
  ()

let erase_check_mk_id file line id underfty =
  (* let () = *)
  (*   Printf.printf "|_ %s _| = %s\n" *)
  (*     (Frontend.Undertype.layout underfty) *)
  (*     (Frontend.Type.layout @@ UT.erase underfty) *)
  (* in *)
  let _ = _check_equality file line NT.eq (UT.erase underfty) id.NL.ty in
  UL.{ ty = underfty; x = id.x }

let hide_depedent_var ctx name ty =
  match List.rev ctx with
  | (name', argty) :: _ when String.equal name name' ->
      UT.exists_quantify_variable_in_ty name argty ty
  | _ -> _failatwith __FILE__ __LINE__ "not a well founded ctx, naming error"

let const_type_infer v =
  let open Value in
  match v with
  | U -> _failatwith __FILE__ __LINE__ ""
  | I n ->
      UT.(
        make_basic "_nu" NT.Ty_int (fun nu ->
            P.(Lit (AOp2 ("==", AVar nu, ACint n)))))
  | B true -> UT.(make_basic "_nu" NT.Ty_int (fun nu -> Lit (AVar nu)))
  | B false -> UT.(make_basic "_nu" NT.Ty_int (fun nu -> Not (Lit (AVar nu))))
  | _ -> _failatwith __FILE__ __LINE__ ""

let rec id_type_infer (ctx : UT.t Typectx.t) (id : NL.id NL.typed) :
    NL.id UL.typed =
  let ty =
    try Prim.get_primitive_under_ty id.x
    with _ ->
      let ty = Typectx.get_ty ctx id.x in
      (* let ty = *)
      (*   UT.( *)
      (*     match ty with *)
      (*     | UnderTy_base { basename; normalty; prop } -> *)
      (*         UnderTy_base *)
      (*           { *)
      (*             basename = id.x; *)
      (*             normalty; *)
      (*             prop = P.subst_id prop basename id.x; *)
      (*           } *)
      (*     | _ -> ty) *)
      (* in *)
      ty
  in
  erase_check_mk_id __FILE__ __LINE__ id ty

and id_type_check (ctx : UT.t Typectx.t) (id : NL.id NL.typed) (ty : UT.t) :
    NL.id UL.typed =
  let id = id_type_infer ctx id in
  let () = Undersub.subtyping_check ctx id.UL.ty ty in
  id

and value_type_infer (ctx : UT.t Typectx.t) (a : NL.value NL.typed) :
    UL.value UL.typed =
  let open NL in
  match a.x with
  | Const v -> UL.{ ty = const_type_infer v; x = Const v }
  | Var x ->
      let x = id_type_infer ctx { ty = a.ty; x } in
      UL.{ ty = x.ty; x = Var x.x }
  | Lam (_, _) ->
      (* NOTE: Can we infer a type of the lambda function without the argment type? *)
      _failatwith __FILE__ __LINE__ "cannot infer under arrow type"
  | Fix _ -> _failatwith __FILE__ __LINE__ "unimp"

and value_type_check (ctx : UT.t Typectx.t) (a : NL.value NL.typed) (ty : UT.t)
    : UL.value UL.typed =
  let open NL in
  match (a.x, ty) with
  | Const _, _ | Var _, _ ->
      let x = value_type_infer ctx a in
      let () = Undersub.subtyping_check ctx x.ty ty in
      x
  | Lam (id, body), UT.(UnderTy_arrow { argname; argty; retty }) ->
      let () = erase_check __FILE__ __LINE__ (argty, id.ty) in
      let retty = UT.subst_id retty argname id.x in
      let ctx' = Typectx.overlap ctx (argty, id.x) in
      let body = term_type_check ctx' body retty in
      { ty; x = Lam ({ ty = argty; x = id.x }, body) }
  | Fix _, _ -> _failatwith __FILE__ __LINE__ "unimp"
  | _, _ -> _failatwith __FILE__ __LINE__ ""

and handle_lettu ctx (tu, args, body) self =
  let open UL in
  let args = List.map (id_type_infer ctx) args in
  let tuty = UT.UnderTy_tuple (List.map (fun x -> x.ty) args) in
  let tu = erase_check_mk_id __FILE__ __LINE__ tu tuty in
  let tu = { x = tu.x; ty = tuty } in
  let ctx' = Typectx.overlap ctx (tu.ty, tu.x) in
  let body = self ctx' body in
  (* TODO: sanity check before hide depedent vars *)
  {
    ty = UT.exists_quantify_variable_in_ty tu.x tu.ty body.ty;
    x = LetTu { tu; args; body };
  }

and handle_letdetu ctx (tu, args, body) self =
  let open UL in
  let tu = id_type_infer ctx tu in
  let argsty =
    match tu.ty with
    | UT.UnderTy_tuple ts -> ts
    | _ -> _failatwith __FILE__ __LINE__ ""
  in
  let args =
    List.map (fun (x, ty) -> erase_check_mk_id __FILE__ __LINE__ x ty)
    @@ List.combine args argsty
  in
  let ctx' =
    List.fold_left
      (fun ctx' id ->
        let ctx' = Typectx.overlap ctx' (id.ty, id.x) in
        ctx')
      ctx args
  in
  let body = self ctx' body in
  (* TODO: sanity check before hide depedent vars *)
  let ty =
    List.fold_right
      (fun id ty -> UT.exists_quantify_variable_in_ty id.x id.ty ty)
      args body.ty
  in
  { ty; x = LetDeTu { tu; args; body } }

and handle_letapp ctx (ret, f, args, body) self =
  let open UL in
  let f = id_type_infer ctx f in
  let fty' = UT.arrow_args_rename (List.map (fun x -> x.NL.x) args) f.ty in
  let argsty, retty = UT.destruct_arrow_tp fty' in
  let args =
    List.map (fun ((argty, _), arg) -> id_type_check ctx arg argty)
    @@ List.combine argsty args
  in
  let ret = erase_check_mk_id __FILE__ __LINE__ ret retty in
  let ctx' = Typectx.overlap ctx (ret.ty, ret.x) in
  let body = self ctx' body in
  (* TODO: sanity check before hide depedent vars *)
  {
    ty = UT.exists_quantify_variable_in_ty ret.x ret.ty body.ty;
    x = LetApp { ret; f; args; body };
  }

and handle_letval ctx (lhs, rhs, body) self =
  let open UL in
  let rhs = value_type_infer ctx rhs in
  let lhs = erase_check_mk_id __FILE__ __LINE__ lhs rhs.ty in
  let ctx' = Typectx.overlap ctx (lhs.ty, lhs.x) in
  let body = self ctx' body in
  (* TODO: sanity check before hide depedent vars *)
  {
    ty = UT.exists_quantify_variable_in_ty lhs.x lhs.ty body.ty;
    x = LetVal { lhs; rhs; body };
  }

and term_type_infer (ctx : UT.t Typectx.t) (a : NL.term NL.typed) :
    UL.term UL.typed =
  let open NL in
  match a.x with
  | V v ->
      let v = value_type_infer ctx { ty = a.ty; x = v } in
      { ty = v.ty; x = V v.x }
  | LetTu { tu; args; body } ->
      handle_lettu ctx (tu, args, body) term_type_infer
  | LetDeTu { tu; args; body } ->
      handle_letdetu ctx (tu, args, body) term_type_infer
  | LetApp { ret; f; args; body } ->
      handle_letapp ctx (ret, f, args, body) term_type_infer
  | LetVal { lhs; rhs; body } ->
      handle_letval ctx (lhs, rhs, body) term_type_infer
  | Ite (id, e1, e2) ->
      let id = id_type_infer ctx id in
      let true_branch_prop x =
        Autov.(Prop.(Lit (AVar { ty = Smtty.Bool; x })))
      in
      let false_branch_prop x =
        Autov.(Prop.(Not (Lit (AVar { ty = Smtty.Bool; x }))))
      in
      let true_branch_ctx =
        Typectx.overlap ctx
          ( UT.base_type_add_conjunction_with_selfname true_branch_prop id.ty,
            id.x )
      in
      let false_branch_ctx =
        Typectx.overlap ctx
          ( UT.base_type_add_conjunction_with_selfname false_branch_prop id.ty,
            id.x )
      in
      let e1 = term_type_infer true_branch_ctx e1 in
      let e2 = term_type_infer false_branch_ctx e2 in
      let tys =
        [
          UT.base_type_add_implication (true_branch_prop id.x) e1.ty;
          UT.base_type_add_implication (false_branch_prop id.x) e2.ty;
        ]
      in
      let () =
        List.iter
          (fun ty ->
            Printf.printf "case ty: %s\n" @@ Frontend.Undertype.pretty_layout ty)
          tys
      in
      let ty = UT.disjunct_list tys in
      let () =
        Printf.printf "merged case ty: %s\n"
        @@ Frontend.Undertype.pretty_layout ty
      in
      (* NUTE: underappproximate here *)
      { ty; x = Ite (id, e1, e2) }
  | Match (id, cases) ->
      let id = id_type_infer ctx id in
      let handle_case { constructor; args; exp } =
        let constructor_ty = Prim.get_primitive_rev_under_ty constructor in
        let retty, args =
          let open UT in
          match constructor_ty with
          | UnderTy_base _ -> (constructor_ty, [])
          | UnderTy_arrow { argty; retty = UnderTy_tuple ts; argname } ->
              let ts = List.map (fun t -> UT.subst_id t argname id.x) ts in
              let tsargs = _safe_combine __FILE__ __LINE__ ts args in
              let args =
                List.map
                  (fun (t, id) ->
                    match t with
                    | UnderTy_base { basename; normalty; prop } ->
                        ( UnderTy_base
                            {
                              basename = id;
                              normalty;
                              prop = P.subst_id prop basename id;
                            },
                          id )
                    | _ -> _failatwith __FILE__ __LINE__ "wrong rev under prim")
                  tsargs
              in
              (argty, args)
          | _ -> _failatwith __FILE__ __LINE__ "wrong rev under prim"
        in
        let ctx' =
          Typectx.overlaps ctx
          @@ UT.
               ( base_type_add_conjunction_with_selfname
                   (fun id ->
                     let basename, prop = base_type_extract_prop retty in
                     P.subst_id prop basename id)
                   id.ty,
                 id.x )
             :: args
        in
        let exp = term_type_infer ctx' exp in
        UL.{ constructor; args = List.map snd args; exp }
      in
      let cases = List.map handle_case cases in
      let tys = List.map UL.(fun x -> x.exp.ty) cases in
      let () =
        List.iter
          (fun ty ->
            Printf.printf "case ty: %s\n" @@ Frontend.Undertype.pretty_layout ty)
          tys
      in
      let ty = UT.disjunct_list tys in
      let () =
        Printf.printf "merged case ty: %s\n"
        @@ Frontend.Undertype.pretty_layout ty
      in
      { ty; x = Match (id, cases) }

and term_type_check (ctx : UT.t Typectx.t) (x : NL.term NL.typed) (ty : UT.t) :
    UL.term UL.typed =
  let () = Printf.printf "%s\n" (layout_judge ctx (x, ty)) in
  let () = erase_check __FILE__ __LINE__ (ty, x.ty) in
  let self ctx e = term_type_check ctx e ty in
  let open NL in
  match (x.x, ty) with
  | V v, _ ->
      let v = value_type_check ctx { ty = x.ty; x = v } ty in
      { ty = v.ty; x = V v.x }
  | LetTu { tu; args; body }, _ -> handle_lettu ctx (tu, args, body) self
  | LetDeTu { tu; args; body }, _ -> handle_letdetu ctx (tu, args, body) self
  | LetApp { ret; f; args; body }, _ ->
      handle_letapp ctx (ret, f, args, body) self
  | LetVal { lhs; rhs; body }, _ -> handle_letval ctx (lhs, rhs, body) self
  | Ite (_, _, _), _ | Match (_, _), _ ->
      let x = term_type_infer ctx x in
      let () = Undersub.subtyping_check ctx x.ty ty in
      (* NOTE: underappproximate here *)
      { ty; x = x.x }

let type_check x ty = term_type_check [] x ty

module SNA = Languages.StrucNA
module SOA = Languages.StrucOA

let struc_check l r =
  let open SNA in
  List.iter
    (fun (name', ty) ->
      match List.find_opt (fun { name; _ } -> String.equal name name') l with
      | None -> _failatwith __FILE__ __LINE__ "does not provide source code"
      | Some { body; _ } ->
          let _ = type_check body ty in
          ())
    r
