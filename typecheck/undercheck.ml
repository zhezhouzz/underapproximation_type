module NL = Languages.NormalAnormal
module UL = Languages.UnderAnormal
module NT = Languages.Normalty
module UT = Languages.Underty
open Zzdatatype.Datatype
open Abstraction
open Sugar

let layout_judge = Frontend.Typectx.pretty_layout_under_judge Trans.nan_to_term
let layout_subtyping = Frontend.Typectx.pretty_layout_under_subtyping

(* TODO: the variables in type context, should be forall or exists? *)
let subtyping_to_query ctx typeself (prop1, prop2) =
  let fv1 = Autov.prop_fv prop1 in
  let fv2 = Autov.prop_fv prop2 in
  let fv = fv1 @ fv2 in
  let ctx =
    List.filter_map
      (fun (x, ty) ->
        UT.(
          match ty with
          | UnderTy_base { basename; prop; _ } ->
              let prop = P.subst_id prop basename x in
              Some (x, prop)
          | _ -> None))
      ctx
  in
  let () =
    List.iter
      (fun name ->
        if
          String.equal typeself name
          || List.exists (fun (x, _) -> String.equal name x) ctx
        then ()
        else
          _failatwith __FILE__ __LINE__
          @@ spf "type context is not well founded, %s not found in ctx" name)
      fv
  in
  let pre = List.map snd ctx in
  let () =
    Printf.printf "SMT check:\n\twith ctx: %s\n\t(%s) => (%s)\n\n"
      (List.split_by " ∧ " Autov.pretty_layout_prop pre)
      (Autov.pretty_layout_prop prop2)
      (Autov.pretty_layout_prop prop1)
  in
  (pre @ [ prop2 ], prop1)

let subtyping_check (ctx : UT.t Typectx.t) (t1 : UT.t) (t2 : UT.t) =
  let open UT in
  let rec aux ctx (t1, t2) =
    let () = Printf.printf "Subtype: %s\n" @@ layout_subtyping ctx (t1, t2) in
    match (t1, t2) with
    | ( UnderTy_base { basename = name1; prop = prop1; _ },
        UnderTy_base { basename = name2; prop = prop2; _ } ) ->
        let typeself, prop1, prop2 =
          match (Typectx.in_ctx ctx name1, Typectx.in_ctx ctx name2) with
          | true, true ->
              ( _check_equality __FILE__ __LINE__ String.equal name1 name2,
                prop1,
                prop2 )
          | false, true -> (name2, P.subst_id prop1 name1 name2, prop2)
          | _, _ -> (name1, prop1, P.subst_id prop2 name2 name1)
        in
        let pres, res = subtyping_to_query ctx typeself (prop1, prop2) in
        if Autov.check_implies_multi_pre pres res then ()
        else failwith "Subtyping check: rejected by the verifier"
    | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
        List.iter (aux ctx) @@ List.combine ts1 ts2
    | ( UnderTy_arrow { argname = x1; argty = t11; retty = t12 },
        UnderTy_arrow { argname = x2; argty = t21; retty = t22 } ) ->
        let t22 = subst_id t22 x2 x1 in
        let () = aux ctx (t21, t11) in
        let () = aux ctx (t12, t22) in
        ()
    | _, _ -> _failatwith __FILE__ __LINE__ "die: under subtype"
  in
  aux ctx (t1, t2)

let erase_check file line (underfty, normalty) =
  (* let () = *)
  (*   Printf.printf "|_ %s _| = %s\n" *)
  (*     (Frontend.Undertype.layout underfty) *)
  (*     (Frontend.Type.layout @@ UT.erase underfty) *)
  (* in *)
  let _ = _check_equality file line NT.eq (UT.erase underfty) normalty in
  ()

let hide_depedent_var ctx name ty =
  match List.rev ctx with
  | (name', argty) :: _ when String.equal name name' ->
      UT.exists_quantify_variable_in_ty name argty ty
  | _ -> _failatwith __FILE__ __LINE__ "not a well founded ctx, naming error"

let value_type_infer v =
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

let rec bidirect_type_infer (ctx : UT.t Typectx.t) (a : NL.term NL.typed) :
    UL.term UL.typed =
  let open NL in
  match a.x with
  | Const v -> UL.{ ty = value_type_infer v; x = Const v }
  | Var x ->
      let x = bidirect_type_infer_id ctx { ty = a.ty; x } in
      UL.{ ty = x.ty; x = Var x.x }
  | Tu es ->
      let es = List.map (bidirect_type_infer_id ctx) es in
      let ty = UT.UnderTy_tuple (List.map (fun x -> x.UL.ty) es) in
      UL.{ ty; x = Tu es }
  | Lam (_, _) ->
      (* NOTE: Can we infer a type of the lambda function without the argment type? *)
      _failatwith __FILE__ __LINE__ "cannot infer under arrow type"
  | Fix _ -> _failatwith __FILE__ __LINE__ "unimp"
  | App (f, args) ->
      let f = bidirect_type_infer_id ctx f in
      let fty' = UT.arrow_args_rename (List.map (fun x -> x.x) args) f.ty in
      let argsty', retty' = UT.destruct_arrow_tp fty' in
      let args =
        List.map (fun ((argty, _), arg) -> bidirect_type_check_id ctx arg argty)
        @@ List.combine argsty' args
      in
      UL.{ ty = retty'; x = App (f, args) }
  | Let (lhs, rhs, body) ->
      let rhs = bidirect_type_infer ctx rhs in
      let lhstys =
        match rhs.ty with
        | UT.UnderTy_tuple ts when List.length ts = List.length lhs -> ts
        | _ -> _failatwith __FILE__ __LINE__ ""
      in
      let lhs =
        List.map (fun (id, idty) ->
            let () = erase_check __FILE__ __LINE__ (idty, id.ty) in
            UL.{ ty = idty; x = id.x })
        @@ List.combine lhs lhstys
      in
      let ctx' =
        List.fold_left
          (fun ctx' id ->
            let ctx' = Typectx.overlap ctx' UL.(id.ty, id.x) in
            ctx')
          ctx lhs
      in
      let body = bidirect_type_infer ctx' body in
      (* TODO: sanity check before hide depedent vars *)
      let ty =
        List.fold_right
          (fun id ty -> UT.exists_quantify_variable_in_ty id.UL.x id.UL.ty ty)
          lhs body.ty
      in
      { ty; x = Let (lhs, rhs, body) }
  | Ite (id, e1, e2) ->
      let id = bidirect_type_infer_id ctx id in
      let true_branch_prop x =
        Autov.(Prop.(Lit (AVar { ty = Smtty.Bool; x })))
      in
      let false_branch_prop x =
        Autov.(Prop.(Not (Lit (AVar { ty = Smtty.Bool; x }))))
      in
      let true_branch_ctx =
        Typectx.overlap ctx
          (UT.base_type_add_conjunction true_branch_prop id.ty, id.x)
      in
      let false_branch_ctx =
        Typectx.overlap ctx
          (UT.base_type_add_conjunction false_branch_prop id.ty, id.x)
      in
      let e1 = bidirect_type_infer true_branch_ctx e1 in
      let e2 = bidirect_type_infer false_branch_ctx e2 in
      let tys = [ e1.ty; e2.ty ] in
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
      let id = bidirect_type_infer_id ctx id in
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
               ( base_type_add_conjunction
                   (fun id ->
                     let basename, prop = base_type_extract_prop retty in
                     P.subst_id prop basename id)
                   id.ty,
                 id.x )
             :: args
        in
        let exp = bidirect_type_infer ctx' exp in
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

and bidirect_type_infer_id (ctx : UT.t Typectx.t) (id : NL.id NL.typed) :
    NL.id UL.typed =
  let ty =
    try Prim.get_primitive_under_ty id.x
    with _ ->
      let ty = Typectx.get_ty ctx id.x in
      let ty =
        UT.(
          match ty with
          | UnderTy_base { basename; normalty; prop } ->
              UnderTy_base
                {
                  basename = id.x;
                  normalty;
                  prop = P.subst_id prop basename id.x;
                }
          | _ -> ty)
      in
      ty
  in
  let () = erase_check __FILE__ __LINE__ (ty, id.ty) in
  UL.{ ty; x = id.x }

and bidirect_type_check_id (ctx : UT.t Typectx.t) (id : NL.id NL.typed)
    (ty : UT.t) : NL.id UL.typed =
  let id = bidirect_type_infer_id ctx id in
  let () = subtyping_check ctx id.UL.ty ty in
  id

and bidirect_type_check (ctx : UT.t Typectx.t) (x : NL.term NL.typed)
    (ty : UT.t) : UL.term UL.typed =
  let () = Printf.printf "%s\n" (layout_judge ctx (x, ty)) in
  let () = erase_check __FILE__ __LINE__ (ty, x.ty) in
  let open NL in
  match (x.x, ty) with
  | Const _, _ -> _failatwith __FILE__ __LINE__ "unimp"
  | Var _, _ ->
      let x = bidirect_type_infer ctx x in
      let () = subtyping_check ctx x.ty ty in
      x
  | Tu es, UT.UnderTy_tuple tys ->
      let estys = _safe_combine __FILE__ __LINE__ es tys in
      let es =
        List.map (fun (e, ty) -> bidirect_type_check_id ctx e ty) estys
      in
      { ty; x = Tu es }
  | Lam (id, body), UT.(UnderTy_arrow { argname; argty; retty }) ->
      let () = erase_check __FILE__ __LINE__ (argty, id.ty) in
      let retty = UT.subst_id retty argname id.x in
      let ctx' = Typectx.overlap ctx (argty, id.x) in
      let body = bidirect_type_check ctx' body retty in
      { ty; x = Lam ({ ty = argty; x = id.x }, body) }
  | Fix _, _ -> _failatwith __FILE__ __LINE__ "unimp"
  (* | App (f, args), ty -> *)
  (*     let f = bidirect_type_infer_id ctx f in *)
  (*     let rec check (ctx', args') (args, underftp) = *)
  (*       match (args, underftp) with *)
  (*       | [], tp -> *)
  (*           let () = subtyping_check ctx' tp ty in *)
  (*           UL.{ ty = tp; x = App ({ ty = underftp; x = f.x }, args') } *)
  (*       | id :: args, UT.(UnderTy_arrow { argname; argty; retty }) -> *)
  (*           let id = bidirect_type_infer_id ctx' id in *)
  (*           let () = subtyping_check ctx' id.ty argty in *)
  (*           let ctx' = Typectx.overlap ctx' (id.ty, id.x) in *)
  (*           let retty = UT.subst_id retty argname id.x in *)
  (*           check (ctx', args' @ [ UL.{ ty = id.ty; x = id.x } ]) (args, retty) *)
  (*       | _ -> _failatwith __FILE__ __LINE__ "app" *)
  (*     in *)
  (*     check (ctx, []) (args, f.ty) *)
  | Let (lhs, rhs, body), ty ->
      let rhs = bidirect_type_infer ctx rhs in
      let lhstys =
        match rhs.ty with
        | UT.UnderTy_tuple ts when List.length ts = List.length lhs -> ts
        | _ when List.length lhs = 1 -> [ rhs.ty ]
        | _ ->
            _failatwith __FILE__ __LINE__
            @@ Printf.sprintf "die:bidirect_type_check let (%s) has type (%s)"
                 (List.split_by_comma
                    (fun { ty; x } ->
                      Printf.sprintf "(%s:%s)" x (Frontend.Type.layout ty))
                    lhs)
                 (Frontend.Undertype.layout rhs.ty)
      in
      let lhs =
        List.map (fun (id, idty) ->
            let () = erase_check __FILE__ __LINE__ (idty, id.ty) in
            UL.{ ty = idty; x = id.x })
        @@ List.combine lhs lhstys
      in
      let ctx' =
        List.fold_left
          (fun ctx' id ->
            let ctx' = Typectx.overlap ctx' UL.(id.ty, id.x) in
            ctx')
          ctx lhs
      in
      let body = bidirect_type_check ctx' body ty in
      { ty = body.ty; x = Let (lhs, rhs, body) }
  | Ite (_, _, _), ty | Match (_, _), ty | App (_, _), ty ->
      let x = bidirect_type_infer ctx x in
      let () = subtyping_check ctx x.ty ty in
      (* NOTE: underappproximate here *)
      { ty; x = x.x }
  | _, _ -> _failatwith __FILE__ __LINE__ "die"

let type_check x ty = bidirect_type_check [] x ty

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
