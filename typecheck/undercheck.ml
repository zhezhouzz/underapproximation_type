module NL = Languages.NormalAnormal
module UL = Languages.UnderAnormal
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

(* type 'a bodyttyped = { bodyt_ty : UT.t; bodyt_x : 'a } *)

(* let unify_to_ctx x ctx = *)
(*   let bodyt_ty, ctx = Qtypectx.unify_raw x.UL.ty ctx in *)
(*   ({ bodyt_ty; bodyt_x = x.UL.x }, ctx) *)

(* let unify_to_ctxs xs ctx = *)
(*   List.fold_right *)
(*     (fun x (xs, ctx) -> *)
(*       let x, ctx = unify_to_ctx x ctx in *)
(*       (x :: xs, ctx)) *)
(*     xs ([], ctx) *)

let layout_term e = Frontend.Expr.layout @@ Trans.nan_to_term e

(* let layout_judge ctx (e, r) = *)
(*   Frontend.Typectx.pretty_layout_over_judge ctx (Trans.nan_to_term e, r) *)

(* let layout_judge ctx (e, ty) = *)
(*   Frontend.Underty.layout_qt *)
(*     (fun ctx -> *)
(*       Frontend.Typectx.pretty_layout_under_judge Trans.nan_to_term ctx (e, ty)) *)
(*     ctx *)

let check_empty_hidden file line hvs =
  if List.length hvs != 0 then
    _failatwith file line "do not allow hidden variables"
  else ()

let lit_to_prop_lit (ty, x) =
  let open UL in
  match x with
  | ConstB b -> P.(ACbool b)
  | ConstI i -> P.(ACint i)
  | Var id -> P.(AVar { ty; x = id })

let erase_check file line (underfty, normalty) =
  (* let () = *)
  (*   Pp.printf "|_ %s _| ~> %s = %s\n" *)
  (*     (Frontend.Underty.layout underfty) *)
  (*     (Frontend.Type.layout @@ UT.erase underfty) *)
  (*     (Frontend.Type.layout normalty) *)
  (* in *)
  let _ = _check_equality file line NT.eq (UT.erase underfty) (snd normalty) in
  ()

let erase_check_mk_id file line id underfty =
  (* let () = *)
  (*   Pp.printf "|_ %s _| ~> %s = %s\n" *)
  (*     (Frontend.Underty.layout underfty) *)
  (*     (Frontend.Type.layout @@ UT.erase underfty) *)
  (*     (Frontend.Type.layout id.NL.ty) *)
  (* in *)
  let _ = _check_equality file line NT.eq (UT.erase underfty) (snd id.NL.ty) in
  UL.{ ty = underfty; x = id.x }

let subtyping_check = Undersub.subtyping_check

(* let subtyping_check__with_hidden_vars = *)
(*   Undersub.subtyping_check_with_hidden_vars *)

let close_term_by_diff ctx' ctx UL.{ ty; x } =
  UL.{ x; ty = Qtypectx.close_by_diff ctx' ctx ty }

(* let close_qterm_by_diff ctx' ctx x = *)
(*   UL.{ x = x.x; ty = Qtypectx.close_qv_by_diff ctx' ctx x.ty } *)

let instantiate_in_qctx (qctx : Qtypectx.t) (quty : QUT.t) : UT.t =
  Qtypectx.instantiate_qvs qctx quty

let rec id_type_infer (ctx : Qtypectx.t) (id : NL.id NL.typed) : UL.id UL.typed
    =
  let ty =
    try Qtypectx.get_ty ctx id.x
    with _ -> instantiate_in_qctx ctx @@ Prim.get_primitive_under_ty id.x
  in
  erase_check_mk_id __FILE__ __LINE__ id ty

and id_type_check (ctx : Qtypectx.t) (id : NL.id NL.typed) (ty : UT.t) :
    NL.id UL.typed =
  let id = id_type_infer ctx id in
  let () = subtyping_check __FILE__ __LINE__ ctx id.UL.ty ty in
  id

and lit_type_infer (ctx : Qtypectx.t) (lit : NL.smt_lit NL.typed) :
    UL.smt_lit UL.typed =
  let open NL in
  let open UT in
  match lit.x with
  | ConstI n -> { ty = make_basic_from_const_int n; x = ConstI n }
  | ConstB b -> { ty = make_basic_from_const_bool b; x = ConstB b }
  | Var id ->
      let id = id_type_infer ctx { ty = lit.ty; x = id } in
      UL.{ ty = id.ty; x = Var id.x }

and value_type_infer (notations_ctx : Simpletypectx.UTSimpleTypectx.t)
    (ctx : Qtypectx.t) (a : NL.value NL.typed) : UL.value UL.typed =
  let aty = a.ty in
  match a.x with
  | NL.Lit lit ->
      let lit = lit_type_infer ctx { ty = aty; x = lit } in
      UL.{ ty = lit.ty; x = Lit lit.x }
  | NL.Lam (id, body) -> (
      match fst id.ty with
      | None -> _failatwith __FILE__ __LINE__ "die"
      | Some typename -> (
          match
            Simpletypectx.UTSimpleTypectx.get_opt notations_ctx typename
          with
          | Some (_, idty) ->
              let id = erase_check_mk_id __FILE__ __LINE__ id idty in
              let ctx' = Qtypectx.add_to_right ctx id in
              let body = term_type_infer notations_ctx ctx' body in
              UL.
                {
                  ty =
                    UnderTy_arrow
                      {
                        argname = id.x;
                        hidden_vars = [];
                        argty = id.ty;
                        retty = body.ty;
                      };
                  x = Lam (id, body);
                }
          | None ->
              _failatwith __FILE__ __LINE__
              @@ spf "cannot find the notation of %s (named %s)" id.x typename))
  | NL.Fix _ -> _failatwith __FILE__ __LINE__ "unimp"

and value_type_check (notations_ctx : Simpletypectx.UTSimpleTypectx.t)
    (ctx : Qtypectx.t) (a : NL.value NL.typed) (ty : UT.t) : UL.value UL.typed =
  let open UT in
  let result =
    match (a.NL.x, ty) with
    | NL.Lit _, _ ->
        let x = value_type_infer notations_ctx ctx a in
        let () = subtyping_check __FILE__ __LINE__ ctx x.ty ty in
        x
    | NL.Lam (id, body), UnderTy_arrow { argname; hidden_vars; argty; retty } ->
        let () =
          match Simpletypectx.UTSimpleTypectx.get_opt notations_ctx id.x with
          | Some _ -> _failatwith __FILE__ __LINE__ "die"
          | None -> ()
        in
        let id = erase_check_mk_id __FILE__ __LINE__ id argty in
        let () = check_empty_hidden __FILE__ __LINE__ hidden_vars in
        let retty = UT.subst_id retty argname id.x in
        let ctx' = Qtypectx.add_to_right ctx id in
        let body = term_type_check notations_ctx ctx' body retty in
        {
          ty = UnderTy_arrow { argname; hidden_vars; argty; retty = body.ty };
          x = Lam (id, body);
        }
    | NL.Fix (f, body), ty ->
        let f = erase_check_mk_id __FILE__ __LINE__ f ty in
        let ctx' = Qtypectx.add_to_right ctx f in
        let body = value_type_check notations_ctx ctx' body ty in
        { ty = body.ty; x = Fix (f, body) }
    | _, _ -> _failatwith __FILE__ __LINE__ ""
  in
  result

and handle_lettu (notations_ctx : Simpletypectx.UTSimpleTypectx.t) ctx
    (tu, args, body) target_type =
  let open UL in
  let args = List.map (id_type_infer ctx) args in
  let tu =
    erase_check_mk_id __FILE__ __LINE__ tu
      (UT.UnderTy_tuple (List.map (fun x -> x.ty) args))
  in
  let ctx' = Qtypectx.add_to_right ctx tu in
  let ty, body =
    match target_type with
    | None ->
        let body = term_type_infer notations_ctx ctx' body in
        (Qtypectx.close_by_diff ctx' ctx body.ty, body)
    | Some ty ->
        let body = term_type_check notations_ctx ctx' body ty in
        (ty, body)
  in
  UL.{ ty; x = LetTu { tu; args; body } }

and handle_letdetu (notations_ctx : Simpletypectx.UTSimpleTypectx.t) ctx
    (tu, args, body) target_type =
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
  let ctx' = Qtypectx.add_to_rights ctx args in
  let ty, body =
    match target_type with
    | None ->
        let body = term_type_infer notations_ctx ctx' body in
        (Qtypectx.close_by_diff ctx' ctx body.ty, body)
    | Some ty ->
        let body = term_type_check notations_ctx ctx' body ty in
        (ty, body)
  in
  UL.{ ty; x = LetDeTu { tu; args; body } }

and handle_letapp (notations_ctx : Simpletypectx.UTSimpleTypectx.t) ctx
    (ret, fty, args, body) target_type =
  let open UL in
  let open UT in
  (* let () = Frontend.Qtypectx.pretty_print ctx in *)
  let args = List.map (id_type_infer ctx) args in
  (* let () = Pp.printf "fty': %s\n" (Frontend.Qunderty.pretty_layout fty') in *)
  (* let () = Pp.printf "%s\n" @@ layout_judge ctx' (body, without_qv fty') in *)
  (* let argsty, retty = UT.destruct_arrow_tp fty' in *)
  let () = Frontend.Qtypectx.pretty_print_app_judge ctx (args, fty) in
  (* arguments type check *)
  let rec aux = function
    | [], ty -> ty
    | arg :: args, UnderTy_arrow { argname; hidden_vars; argty; retty } ->
        let hidden_vars, argty =
          Qtypectx.rename_hidden_vars ctx (hidden_vars, argty)
        in
        (* HACK *)
        let () = check_empty_hidden __FILE__ __LINE__ hidden_vars in
        let () =
          if UT.is_fv_in argname retty then
            subtyping_check __FILE__ __LINE__ ctx argty arg.ty
          else subtyping_check __FILE__ __LINE__ ctx arg.ty argty
        in
        let retty = subst_id retty argname arg.x in
        aux (args, retty)
    | _, _ -> _failatwith __FILE__ __LINE__ ""
  in
  let retty = aux (args, fty) in
  let ret = erase_check_mk_id __FILE__ __LINE__ ret retty in
  (* let () = Pp.printf "before handel let-- (%s)\n" ret.NL.x in *)
  (* let retty = *)
  (*   List.fold_left *)
  (*     (fun ret ((_, x), arg) -> UT.subst_id ret arg.bodyt_x x) *)
  (*     retty *)
  (*   @@ List.combine argsty args *)
  (* in *)
  (* let () = Pp.printf "let bind var: %s\n" ret.x in *)
  (* let () = Pp.printf "ctx': %s\n" @@ Frontend.Qtypectx.pretty_layout ctx' in *)
  let ctx' = Qtypectx.add_to_right ctx ret in
  (* let () = Pp.printf "ret.ty: %s\n" (Frontend.Qunderty.layout ret.ty) in *)
  let ty, body =
    match target_type with
    | None ->
        let body = term_type_infer notations_ctx ctx' body in
        (Qtypectx.close_by_diff ctx' ctx body.ty, body)
    | Some ty ->
        let body = term_type_check notations_ctx ctx' body ty in
        (ty, body)
  in
  (ty, (ret, args, body))

and handle_letval (notations_ctx : Simpletypectx.UTSimpleTypectx.t) ctx
    (lhs, rhs, body) target_type =
  let open UL in
  let rhs = value_type_infer notations_ctx ctx rhs in
  let lhs = erase_check_mk_id __FILE__ __LINE__ lhs rhs.ty in
  let ctx' = Qtypectx.add_to_right ctx lhs in
  (* let () = *)
  (*   Pp.printf "after let value ctx: %s\n" *)
  (*     (Frontend.Qtypectx.pretty_layout ctx') *)
  (* in *)
  let ty, body =
    match target_type with
    | None ->
        let body = term_type_infer notations_ctx ctx' body in
        (Qtypectx.close_by_diff ctx' ctx body.ty, body)
    | Some ty ->
        let body = term_type_check notations_ctx ctx' body ty in
        (ty, body)
  in
  UL.{ ty; x = LetVal { lhs; rhs; body } }

and term_type_infer (notations_ctx : Simpletypectx.UTSimpleTypectx.t)
    (ctx : Qtypectx.t) (a : NL.term NL.typed) : UL.term UL.typed =
  let open NL in
  let res =
    match a.x with
    | V v ->
        let v = value_type_infer notations_ctx ctx { ty = a.ty; x = v } in
        UL.{ ty = v.ty; x = V v.x }
    | LetTu { tu; args; body } ->
        handle_lettu notations_ctx ctx (tu, args, body) None
    | LetDeTu { tu; args; body } ->
        handle_letdetu notations_ctx ctx (tu, args, body) None
    | LetOp { ret; op; args; body } ->
        (* let argsty = List.map (fun x -> x.ty) args in *)
        let opty =
          instantiate_in_qctx ctx
          @@ Prim.get_primitive_under_ty (Op.op_to_string op)
        in
        let ty, (ret, args, body) =
          handle_letapp notations_ctx ctx (ret, opty, args, body) None
        in
        { ty; x = LetOp { ret; op; args; body } }
    | LetApp { ret; f; args; body } ->
        let f = id_type_infer ctx f in
        let ty, (ret, args, body) =
          handle_letapp notations_ctx ctx (ret, f.ty, args, body) None
        in
        { ty; x = LetApp { ret; f; args; body } }
    | LetVal { lhs; rhs; body } ->
        handle_letval notations_ctx ctx (lhs, rhs, body) None
    | Ite { cond; e_t; e_f } ->
        let () =
          Pp.printf "@{<bold>Before If@}\n";
          Frontend.Qtypectx.pretty_print ctx
        in
        let cond = id_type_infer ctx cond in
        let handle_case propf e =
          let ctx' =
            Qtypectx.conjunct ctx
              (cond.x, UT.make_basic_from_prop NT.Ty_bool propf)
          in
          close_term_by_diff ctx' ctx (term_type_infer notations_ctx ctx' e)
        in
        let e_t = handle_case (fun x -> P.(Lit (AVar x))) e_t in
        let e_f = handle_case (fun x -> P.(Not (Lit (AVar x)))) e_f in
        (* let () = *)
        (*   Pp.printf "@{<bold>Compare@}\n"; *)
        (*   Frontend.Qtypectx.pretty_print ctx; *)
        (*   Frontend.Qtypectx.pretty_print true_branch_ctx *)
        (* in *)
        let () =
          List.iter
            (fun ty ->
              Pp.printf "case ty: %s\n" @@ Frontend.Underty.pretty_layout ty)
            [ e_t.ty; e_f.ty ]
        in
        let ty = UT.disjunct_list [ e_t.ty; e_f.ty ] in
        let () =
          Pp.printf "merged case ty: %s\n" @@ Frontend.Underty.pretty_layout ty
        in
        (* NOTE: underappproximate here *)
        { ty; x = UL.Ite { cond; e_t; e_f } }
    | Match { matched; cases } ->
        let matched = id_type_infer ctx matched in
        let handle_case { constructor; args; exp } =
          (* let rev_constructor_nt = *)
          (*   let argsty, retty = NT.destruct_arrow_tp constructor.ty in *)
          (*   match argsty with *)
          (*   | [] -> retty *)
          (*   | _ -> NT.(Ty_arrow (retty, Ty_tuple argsty)) *)
          (* in *)
          let constructor_ty =
            instantiate_in_qctx ctx
            @@ Prim.get_primitive_rev_under_ty constructor.x
          in
          let constructor = UL.{ ty = constructor_ty; x = constructor.x } in
          let hidden_vars, retty, args =
            let open UT in
            match constructor.ty with
            | UnderTy_base _ -> ([], constructor.ty, [])
            | UnderTy_arrow
                { argname; argty; hidden_vars; retty = UnderTy_tuple ts } ->
                let ts =
                  List.map (fun t -> UT.subst_id t argname matched.x) ts
                in
                let tsargs = _safe_combine __FILE__ __LINE__ ts args in
                let args =
                  List.map
                    (fun (t, id) ->
                      match t with
                      | UnderTy_base { basename; normalty; prop } ->
                          UL.
                            {
                              ty =
                                UT.make_basic_from_prop normalty (fun x ->
                                    P.subst_id prop basename x.x);
                              x = id;
                            }
                      | _ ->
                          _failatwith __FILE__ __LINE__ "wrong rev under prim")
                    tsargs
                in
                (hidden_vars, argty, args)
            | _ -> _failatwith __FILE__ __LINE__ "wrong rev under prim"
          in
          (* let ctx', retty =  *)
          (*   Qtypectx.add_hidden_vars_to_right ctx (hidden_vars, retty) *)
          (* in *)
          let () = check_empty_hidden __FILE__ __LINE__ hidden_vars in
          let ctx' = Qtypectx.conjunct ctx (matched.x, retty) in
          let ctx' = Qtypectx.add_to_rights ctx' args in
          let exp = term_type_infer notations_ctx ctx' exp in
          ( Qtypectx.close_by_diff ctx' ctx exp.ty,
            UL.{ constructor; args = List.map (fun x -> x.x) args; exp } )
        in
        let tys, cases = List.split @@ List.map handle_case cases in
        let () =
          List.iter
            (fun ty ->
              Pp.printf "case ty: %s\n" @@ Frontend.Underty.pretty_layout ty)
            tys
        in
        let ty = UT.disjunct_list tys in
        let () =
          Pp.printf "merged case ty: %s\n" @@ Frontend.Underty.pretty_layout ty
        in
        { ty; x = Match { matched; cases } }
  in
  let () =
    Frontend.Qtypectx.pretty_print_infer ctx (layout_term a, res.UL.ty)
  in
  res

and term_type_check (notations_ctx : Simpletypectx.UTSimpleTypectx.t)
    (ctx : Qtypectx.t) (x : NL.term NL.typed) (ty : UT.t) : UL.term UL.typed =
  let () = Frontend.Qtypectx.pretty_print_judge ctx (layout_term x, ty) in
  let () = erase_check __FILE__ __LINE__ (ty, x.ty) in
  let open NL in
  match (x.x, ty) with
  | V v, _ ->
      let v = value_type_check notations_ctx ctx { ty = x.ty; x = v } ty in
      { ty = v.ty; x = V v.x }
  | LetTu { tu; args; body }, _ ->
      handle_lettu notations_ctx ctx (tu, args, body) (Some ty)
  | LetDeTu { tu; args; body }, _ ->
      handle_letdetu notations_ctx ctx (tu, args, body) (Some ty)
  | LetApp { ret; f; args; body }, _ ->
      let f = id_type_infer ctx f in
      let ty, (ret, args, body) =
        handle_letapp notations_ctx ctx (ret, f.ty, args, body) (Some ty)
      in
      { ty; x = LetApp { ret; f; args; body } }
  | LetOp { ret; op; args; body }, _ ->
      (* let argsty = List.map (fun x -> x.ty) args in *)
      let opty =
        instantiate_in_qctx ctx
        @@ Prim.get_primitive_under_ty (Op.op_to_string op)
      in
      let ty, (ret, args, body) =
        handle_letapp notations_ctx ctx (ret, opty, args, body) (Some ty)
      in
      (* let _ = *)
      (*   Pp.printf "ret = _:%s\n" @@ Frontend.Underty.pretty_layout ty' *)
      (* in *)
      { ty; x = LetOp { ret; op; args; body } }
  | LetVal { lhs; rhs; body }, _ ->
      handle_letval notations_ctx ctx (lhs, rhs, body) (Some ty)
  | Ite _, _ | Match _, _ ->
      let x = term_type_infer notations_ctx ctx x in
      let () = subtyping_check __FILE__ __LINE__ ctx x.ty ty in
      (* NOTE: underappproximate here *)
      { ty; x = x.x }

let type_check (notations_ctx : Simpletypectx.UTSimpleTypectx.t) x ty =
  let ctx, ty = Qtypectx.mk_from_qunder ty in
  term_type_check notations_ctx ctx x ty

module SNA = Languages.StrucNA
module SOA = Languages.StrucOA

let struc_check l notations r =
  let open SNA in
  List.iteri
    (fun id (name', ty) ->
      let () = Pp.printf "@{<bold>Task %i:@}\n" id in
      match List.find_opt (fun { name; _ } -> String.equal name name') l with
      | None -> _failatwith __FILE__ __LINE__ "does not provide source code"
      | Some { body; _ } ->
          let notations_ctx =
            Simpletypectx.UTSimpleTypectx.(
              List.fold_left
                (* TODO: quantifiers? *)
                  (fun ctx (name, ty) ->
                  add_to_right ctx (ty.Qunder.qbody, name))
                empty notations)
          in
          let _ = type_check notations_ctx body ty in
          ())
    r
