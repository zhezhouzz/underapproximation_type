module NL = Languages.NormalAnormal
module UL = Languages.QunderAnormal
module NT = Languages.Normalty
module UT = Languages.Underty
module QUT = Languages.Qunderty
module Typectx = Languages.UnderTypectx
module Qtypectx = Languages.Qtypectx
module Op = Languages.Op
module P = Autov.Prop
open Zzdatatype.Datatype
open Abstraction
open Sugar
open QUT

type 'a bodyttyped = { bodyt_ty : UT.t; bodyt_x : 'a }

let unify_to_ctx x ctx =
  let bodyt_ty, ctx = Qtypectx.unify_raw x.UL.ty ctx in
  ({ bodyt_ty; bodyt_x = x.UL.x }, ctx)

let unify_to_ctxs xs ctx =
  List.fold_right
    (fun x (xs, ctx) ->
      let x, ctx = unify_to_ctx x ctx in
      (x :: xs, ctx))
    xs ([], ctx)

let layout_term e = Frontend.Expr.layout @@ Trans.nan_to_term e

(* let layout_judge ctx (e, r) = *)
(*   Frontend.Typectx.pretty_layout_over_judge ctx (Trans.nan_to_term e, r) *)

(* let layout_judge ctx (e, ty) = *)
(*   Frontend.Underty.layout_qt *)
(*     (fun ctx -> *)
(*       Frontend.Typectx.pretty_layout_under_judge Trans.nan_to_term ctx (e, ty)) *)
(*     ctx *)

let lit_to_prop_lit (ty, x) =
  let open UL in
  match x with
  | ConstB b -> P.(ACbool b)
  | ConstI i -> P.(ACint i)
  | Var id -> P.(AVar { ty; x = id })

let erase_check file line (underfty, normalty) =
  (* let () = *)
  (*   Printf.printf "|_ %s _| ~> %s = %s\n" *)
  (*     (Frontend.Underty.layout underfty) *)
  (*     (Frontend.Type.layout @@ UT.erase underfty) *)
  (*     (Frontend.Type.layout normalty) *)
  (* in *)
  let _ = _check_equality file line NT.eq (UT.erase underfty) normalty in
  ()

let erase_check_mk_id file line id underfty =
  (* let () = *)
  (*   Printf.printf "|_ %s _| ~> %s = %s\n" *)
  (*     (Frontend.Underty.layout underfty) *)
  (*     (Frontend.Type.layout @@ UT.erase underfty) *)
  (*     (Frontend.Type.layout id.NL.ty) *)
  (* in *)
  let _ =
    _check_equality file line NT.eq (UT.erase underfty.QUT.qbody) id.NL.ty
  in
  UL.{ ty = underfty; x = id.x }

let subtyping_check = Undersub.subtyping_check

(* let close_qv_by_ctx_diff (ctx, UL.{ ty; x }) ctx' = *)
(*   UL.{ ty = Qtypectx.close_qv_by_diff ctx ctx' ty; x } *)

(* let add_qv_by_ctx_diff (ctx, { bodyt_ty; bodyt_x }) ctx' = *)
(*   UL. *)
(*     { *)
(*       ty = Qtypectx.close_qv_by_diff ctx ctx' (QUT.without_qv bodyt_ty); *)
(*       x = bodyt_x; *)
(*     } *)

(* let hide_vars_in_ctx ctx ty vars = *)
(*   List.fold_right *)
(*     (fun id { uqvs; eqvs; qbody } -> *)
(*       let idty = Typectx.get_ty ctx id in *)
(*       { uqvs; eqvs; qbody = UT.hide_quantify_variable_in_bodyt id idty qbody }) *)
(*     vars ty *)

let close_term_by_diff ctx' ctx { bodyt_ty; bodyt_x } =
  UL.
    {
      x = bodyt_x;
      ty = Qtypectx.close_qv_by_diff ctx' ctx (without_qv bodyt_ty);
    }

let close_qterm_by_diff ctx' ctx x =
  UL.{ x = x.x; ty = Qtypectx.close_qv_by_diff ctx' ctx x.ty }

let rec id_type_infer (ctx : Qtypectx.t) (id : NL.id NL.typed) : UL.id UL.typed
    =
  let ty =
    try without_qv (Typectx.get_ty ctx.qbody id.x)
    with _ -> Prim.get_primitive_under_ty (External id.x)
  in
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
  erase_check_mk_id __FILE__ __LINE__ id ty

and id_type_check (ctx : Qtypectx.t) (id : NL.id NL.typed) (ty : QUT.t) :
    NL.id UL.typed =
  let id = id_type_infer ctx id in
  let () = subtyping_check __FILE__ __LINE__ ctx id.UL.ty ty in
  id

and lit_type_infer (ctx : Qtypectx.t) (lit : NL.smt_lit NL.typed) :
    UL.smt_lit UL.typed =
  let open NL in
  let open UT in
  match lit.x with
  | ConstI n ->
      {
        ty =
          without_qv
          @@ make_basic "_nu" NT.Ty_int (fun nu ->
                 P.(Lit (AOp2 ("==", AVar nu, ACint n))));
        x = ConstI n;
      }
  | ConstB true ->
      {
        ty = without_qv @@ make_basic "_nu" NT.Ty_int (fun nu -> Lit (AVar nu));
        x = ConstB true;
      }
  | ConstB false ->
      {
        ty =
          without_qv
          @@ make_basic "_nu" NT.Ty_int (fun nu -> Not (Lit (AVar nu)));
        x = ConstB true;
      }
  | Var id ->
      let id = id_type_infer ctx { ty = lit.ty; x = id } in
      UL.{ ty = id.ty; x = Var id.x }

and value_type_infer (ctx : Qtypectx.t) (a : NL.value NL.typed) :
    UL.value UL.typed =
  let aty = a.ty in
  match a.x with
  | NL.Lit lit ->
      let lit = lit_type_infer ctx { ty = aty; x = lit } in
      UL.{ ty = lit.ty; x = Lit lit.x }
  | NL.Lam (_, _) ->
      (* NOTE: Can we infer a type of the lambda function without the argment type? *)
      _failatwith __FILE__ __LINE__ "cannot infer under arrow type"
  | NL.Fix _ -> _failatwith __FILE__ __LINE__ "unimp"

and value_type_check (ctx : Qtypectx.t) (a : NL.value NL.typed) (ty : QUT.t) :
    UL.value UL.typed =
  let open UT in
  let ty, ctx' = Qtypectx.unify_raw ty ctx in
  let result =
    match (a.NL.x, ty) with
    | NL.Lit _, _ ->
        let x = value_type_infer ctx' a in
        let () = subtyping_check __FILE__ __LINE__ ctx' x.ty (without_qv ty) in
        x
    | NL.Lam (id, body), UnderTy_arrow { argname; argty; retty } ->
        let () = erase_check __FILE__ __LINE__ (argty, id.ty) in
        let retty = UT.subst_id retty argname id.x in
        let ctx' = Qtypectx.add_to_right ctx' (argty, id.x) in
        let body = term_type_check ctx' body (without_qv retty) in
        {
          ty =
            QUT.map
              (fun retty -> UnderTy_arrow { argname; argty; retty })
              body.ty;
          x = Lam ({ ty = without_qv @@ argty; x = id.x }, body);
        }
    | NL.Fix (f, body), ty ->
        let () = erase_check __FILE__ __LINE__ (ty, f.ty) in
        let ctx' = Qtypectx.add_to_right ctx' (ty, f.x) in
        let body = value_type_check ctx' body (without_qv ty) in
        { ty = body.ty; x = Fix ({ ty = without_qv ty; x = f.x }, body) }
    | _, _ -> _failatwith __FILE__ __LINE__ ""
  in
  close_qterm_by_diff ctx' ctx result

and handle_lettu ctx (tu, args, body) target_type =
  let open UL in
  let args = List.map (id_type_infer ctx) args in
  let tu =
    erase_check_mk_id __FILE__ __LINE__ tu
      (QUT.t_to_tuple_t (List.map (fun x -> x.ty) args))
  in
  let tu, ctx' = unify_to_ctx tu ctx in
  let ctx' = Qtypectx.add_to_right ctx' (tu.bodyt_ty, tu.bodyt_x) in
  let body =
    match target_type with
    | None -> term_type_infer ctx' body
    | Some ty -> term_type_check ctx' body ty
  in
  (* TODO: sanity check before hide depedent vars *)
  let tu = close_term_by_diff ctx' ctx tu in
  close_qterm_by_diff ctx' ctx UL.{ ty = body.ty; x = LetTu { tu; args; body } }

and handle_letdetu ctx (tu, args, body) target_type =
  let open UL in
  let tu = id_type_infer ctx tu in
  let argsty =
    match QUT.distruct_tuple_t_opt tu.ty with
    | Some ts -> ts
    | _ -> _failatwith __FILE__ __LINE__ ""
  in
  let args =
    List.map (fun (x, ty) -> erase_check_mk_id __FILE__ __LINE__ x ty)
    @@ List.combine args argsty
  in
  let args, ctx' =
    List.fold_right
      (fun x (args, ctx) ->
        let x, ctx' = unify_to_ctx x ctx in
        let ctx' = Qtypectx.add_to_right ctx' (x.bodyt_ty, x.bodyt_x) in
        let x = close_term_by_diff ctx' ctx x in
        (x :: args, ctx'))
      args ([], ctx)
  in
  let body =
    match target_type with
    | None -> term_type_infer ctx' body
    | Some ty -> term_type_check ctx' body ty
  in
  (* TODO: sanity check before hide depedent vars *)
  (* let ty = *)
  (*   hide_vars_in_ctx ctx'.qbody body.ty (List.map (fun x -> x.bodyt_x) args) *)
  (* in *)
  (* TODO: handle args correctly ?? *)
  (* let args = List.map (fun x -> add_qv_by_ctx_diff (ctx', x) ctx) args in *)
  close_qterm_by_diff ctx' ctx
    UL.{ ty = body.ty; x = LetDeTu { tu; args; body } }

and handle_letapp ctx (ret, fty, args, body) target_type =
  let open UL in
  let open UT in
  let args = List.map (id_type_infer ctx) args in
  let args, ctx' = unify_to_ctxs args ctx in
  let qargs = List.map (fun arg -> close_term_by_diff ctx' ctx arg) args in
  (* let fty = { fty with uqvs = []; eqvs = fty.uqvs @ fty.eqvs } in *)
  let fty', ctx' = Qtypectx.unify fty ctx' in
  let () = Printf.printf "fty': %s\n" (Frontend.Qunderty.pretty_layout fty') in
  (* let () = Printf.printf "%s\n" @@ layout_judge ctx' (body, without_qv fty') in *)
  (* let argsty, retty = UT.destruct_arrow_tp fty' in *)
  let () = Printf.printf "start type check for let %s\n" ret.NL.x in
  let rec aux ctx' = function
    | [], ty -> (ctx', ty)
    | arg :: args, UnderTy_arrow { argname; argty; retty } ->
        let () =
          subtyping_check __FILE__ __LINE__ ctx' (without_qv arg.bodyt_ty)
            (without_qv argty)
        in
        let retty = subst_id retty argname arg.bodyt_x in
        let ctx' =
          Qtypectx.map
            (fun ctx' -> Typectx.conjunct ctx' (arg.bodyt_x, argty))
            ctx'
        in
        aux ctx' (args, retty)
    | _, _ -> _failatwith __FILE__ __LINE__ ""
  in
  let ctx', retty = aux ctx' (args, fty'.qbody) in
  let _ = erase_check __FILE__ __LINE__ (retty, ret.NL.ty) in
  let () = Printf.printf "before handel let (%s)\n" ret.NL.x in
  (* let retty = *)
  (*   List.fold_left *)
  (*     (fun ret ((_, x), arg) -> UT.subst_id ret arg.bodyt_x x) *)
  (*     retty *)
  (*   @@ List.combine argsty args *)
  (* in *)
  let () = Printf.printf "let bind var: %s\n" ret.x in
  let () = Printf.printf "ctx': %s\n" @@ Frontend.Qtypectx.pretty_layout ctx' in
  let ctx' = Qtypectx.add_to_right ctx (retty, ret.x) in
  let ret =
    close_qterm_by_diff ctx' ctx
      (erase_check_mk_id __FILE__ __LINE__ ret @@ without_qv retty)
  in
  let () = Printf.printf "ret.ty: %s\n" (Frontend.Qunderty.layout ret.ty) in
  let body =
    match target_type with
    | None -> term_type_infer ctx' body
    | Some ty -> term_type_check ctx' body ty
  in
  (* TODO: sanity check before hide depedent vars *)
  (Qtypectx.close_qv_by_diff ctx' ctx body.ty, (ret, qargs, body))

and handle_letval ctx (lhs, rhs, body) target_type =
  let open UL in
  let rhs = value_type_infer ctx rhs in
  let lhs = erase_check_mk_id __FILE__ __LINE__ lhs rhs.ty in
  let lhsty, ctx' = Qtypectx.unify_raw lhs.ty ctx in
  let ctx' = Qtypectx.add_to_right ctx' (lhsty, lhs.x) in
  let body =
    match target_type with
    | None -> term_type_infer ctx' body
    | Some ty -> term_type_check ctx' body ty
  in
  (* TODO: sanity check before hide depedent vars *)
  close_qterm_by_diff ctx' ctx { ty = body.ty; x = LetVal { lhs; rhs; body } }

and term_type_infer (ctx : Qtypectx.t) (a : NL.term NL.typed) : UL.term UL.typed
    =
  let open NL in
  match a.x with
  | V v ->
      let v = value_type_infer ctx { ty = a.ty; x = v } in
      { ty = v.ty; x = V v.x }
  | LetTu { tu; args; body } -> handle_lettu ctx (tu, args, body) None
  | LetDeTu { tu; args; body } -> handle_letdetu ctx (tu, args, body) None
  | LetOp { ret; op; args; body } ->
      let argsty = List.map (fun x -> x.ty) args in
      let opty =
        Prim.get_primitive_under_ty
          (Op.PrimOp (op, NT.construct_arrow_tp (argsty, ret.ty)))
      in
      let ty, (ret, args, body) =
        handle_letapp ctx (ret, opty, args, body) None
      in
      { ty; x = LetOp { ret; op; args; body } }
  | LetApp { ret; f; args; body } ->
      let f = id_type_infer ctx f in
      let ty, (ret, args, body) =
        handle_letapp ctx (ret, f.ty, args, body) None
      in
      { ty; x = LetApp { ret; f; args; body } }
  | LetVal { lhs; rhs; body } -> handle_letval ctx (lhs, rhs, body) None
  | Ite { cond; e_t; e_f } ->
      let cond = id_type_infer ctx cond in
      let cond, ctx' = unify_to_ctx cond ctx in
      let true_branch_prop x =
        Autov.(Prop.(Lit (AVar { ty = Smtty.Bool; x })))
      in
      let false_branch_prop x =
        Autov.(Prop.(Not (Lit (AVar { ty = Smtty.Bool; x }))))
      in
      let true_branch_ctx =
        Qtypectx.add_to_right ctx'
          ( UT.base_type_add_conjunction_with_selfname true_branch_prop
              cond.bodyt_ty,
            cond.bodyt_x )
      in
      let false_branch_ctx =
        Qtypectx.add_to_right ctx'
          ( UT.base_type_add_conjunction_with_selfname false_branch_prop
              cond.bodyt_ty,
            cond.bodyt_x )
      in
      let e_t, ctx' = unify_to_ctx (term_type_infer true_branch_ctx e_t) ctx' in
      let e_f, ctx' =
        unify_to_ctx (term_type_infer false_branch_ctx e_f) ctx'
      in
      let tys =
        [
          UT.base_type_add_conjunction
            (true_branch_prop cond.bodyt_x)
            e_t.bodyt_ty;
          UT.base_type_add_conjunction
            (false_branch_prop cond.bodyt_x)
            e_f.bodyt_ty;
        ]
      in
      let () =
        List.iter
          (fun ty ->
            Printf.printf "case ty: %s\n" @@ Frontend.Underty.pretty_layout ty)
          tys
      in
      let ty = UT.disjunct_list tys in
      let () =
        Printf.printf "merged case ty: %s\n"
        @@ Frontend.Underty.pretty_layout ty
      in
      let cond = close_term_by_diff ctx' ctx cond in
      let e_t, e_f =
        Sugar.map2 (fun x -> close_term_by_diff ctx' ctx x) (e_t, e_f)
      in
      (* NOTE: underappproximate here *)
      close_term_by_diff ctx' ctx
        { bodyt_ty = ty; bodyt_x = UL.Ite { cond; e_t; e_f } }
  | Match { matched; cases } ->
      let matched = id_type_infer ctx matched in
      let matched, ctx' = unify_to_ctx matched ctx in
      let handle_case { constructor; args; exp } =
        let rev_constructor_nt =
          let argsty, retty = NT.destruct_arrow_tp constructor.ty in
          match argsty with
          | [] -> retty
          | _ -> NT.(Ty_arrow (retty, Ty_tuple argsty))
        in
        let constructor_ty =
          Prim.get_primitive_rev_under_ty
            Op.(PrimOp (Dt constructor.x, rev_constructor_nt))
        in
        let constructor = UL.{ ty = constructor_ty; x = constructor.x } in
        let constructor, ctx' = unify_to_ctx constructor ctx' in
        let retty, args =
          let open UT in
          match constructor.bodyt_ty with
          | UnderTy_base _ -> (constructor.bodyt_ty, [])
          | UnderTy_arrow { argty; retty = UnderTy_tuple ts; argname } ->
              let ts =
                List.map (fun t -> UT.subst_id t argname matched.bodyt_x) ts
              in
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
          Qtypectx.map
            (fun ctx -> Typectx.conjunct ctx (matched.bodyt_x, retty))
            ctx'
        in
        let ctx' = Qtypectx.add_to_rights ctx' args in
        (* let branch_prop id = *)
        (*   let basename, prop = UT.base_type_extract_prop retty in *)
        (*   P.subst_id prop basename id *)
        (* in *)
        (* let ctx' = *)
        (*   Qtypectx.add_to_rights ctx' *)
        (*     (( base_type_add_conjunction_with_selfname branch_prop *)
        (*          matched.bodyt_ty, *)
        (*        matched.bodyt_x ) *)
        (*     :: args) *)
        (* in *)
        let exp = term_type_infer ctx' exp in
        let exp, ctx' = unify_to_ctx exp ctx' in
        (* let casety = *)
        (*   UT.base_type_add_conjunction *)
        (*     (branch_prop matched.bodyt_x) *)
        (*     exp.bodyt_ty *)
        (* in *)
        ( Qtypectx.close_qv_by_diff ctx' ctx (without_qv exp.bodyt_ty),
          UL.
            {
              constructor = close_term_by_diff ctx' ctx constructor;
              args = List.map snd args;
              exp = close_term_by_diff ctx' ctx exp;
            } )
      in
      let tys, cases = List.split @@ List.map handle_case cases in
      let () =
        List.iter
          (fun ty ->
            Printf.printf "case ty: %s\n" @@ Frontend.Qunderty.pretty_layout ty)
          tys
      in
      let ty = QUT.disjunct_list_q tys in
      let () =
        Printf.printf "merged case ty: %s\n"
        @@ Frontend.Qunderty.pretty_layout ty
      in
      { ty; x = Match { matched = close_term_by_diff ctx' ctx matched; cases } }

and term_type_check (ctx : Qtypectx.t) (x : NL.term NL.typed) (ty : QUT.t) :
    UL.term UL.typed =
  let () =
    Printf.printf "%s\n"
      (Frontend.Qtypectx.pretty_layout_judge ctx (layout_term x, ty))
  in
  let () = erase_check __FILE__ __LINE__ (ty.qbody, x.ty) in
  let open NL in
  match (x.x, ty) with
  | V v, _ ->
      let v = value_type_check ctx { ty = x.ty; x = v } ty in
      { ty = v.ty; x = V v.x }
  | LetTu { tu; args; body }, _ -> handle_lettu ctx (tu, args, body) (Some ty)
  | LetDeTu { tu; args; body }, _ ->
      handle_letdetu ctx (tu, args, body) (Some ty)
  | LetApp { ret; f; args; body }, _ ->
      let f = id_type_infer ctx f in
      let ty, (ret, args, body) =
        handle_letapp ctx (ret, f.ty, args, body) (Some ty)
      in
      { ty; x = LetApp { ret; f; args; body } }
  | LetOp { ret; op; args; body }, _ ->
      let argsty = List.map (fun x -> x.ty) args in
      let opty =
        Prim.get_primitive_under_ty
          (Op.PrimOp (op, NT.construct_arrow_tp (argsty, ret.ty)))
      in
      let () = Printf.printf "before handel let (%s)\n" ret.x in
      let ty, (ret, args, body) =
        handle_letapp ctx (ret, opty, args, body) (Some ty)
      in
      (* let _ = *)
      (*   Printf.printf "ret = _:%s\n" @@ Frontend.Underty.pretty_layout ty' *)
      (* in *)
      { ty; x = LetOp { ret; op; args; body } }
  | LetVal { lhs; rhs; body }, _ -> handle_letval ctx (lhs, rhs, body) (Some ty)
  | Ite _, _ | Match _, _ ->
      let x = term_type_infer ctx x in
      let () = subtyping_check __FILE__ __LINE__ ctx x.ty ty in
      (* NOTE: underappproximate here *)
      { ty; x = x.x }

let type_check x ty = term_type_check Qtypectx.empty x ty

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
