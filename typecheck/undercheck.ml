module NL = Languages.NormalAnormal
module UL = Languages.UnderAnormal
module NT = Languages.Normalty
module UT = Languages.Underty
open Zzdatatype.Datatype
open Abstraction

let layout_judge = Frontend.Undertype.pretty_layout_judge Trans.nan_to_term

(* let typectx_well_founded_overlap ctx (ty, name) = *)
(*   let open UT in *)
(*   let rec aux (ty, name) = *)
(*     let ty = *)
(*     match ty with *)
(*     | UnderTy_base {basename; normalty; prop;} -> *)
(*       if String.equal basename name then ty else *)
(*         UnderTy_base {basename = name; normalty; prop = P.subst_id prop basename name} *)
(*     | UnderTy_tuple ts ->  *)

let subtyping_to_query ctx (prop1, prop2) =
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
        if List.exists (fun (x, _) -> String.equal name x) ctx then ()
        else failwith "type context is not well founded")
      fv
  in
  let pre = List.map snd ctx in
  let () =
    Printf.printf "SMT check:\n\twith ctx: %s\n\t(%s) => (%s)\n\n"
      (List.split_by " ∧ " Autov.pretty_layout_prop pre)
      (Autov.pretty_layout_prop prop1)
      (Autov.pretty_layout_prop prop2)
  in
  (pre @ [ prop2 ], prop1)

let subtyping_check (ctx : UT.t Typectx.t) (t1 : UT.t) (t2 : UT.t) =
  let open UT in
  let rec aux ctx (t1, t2) =
    (* let () = *)
    (*   Printf.printf "%s ⊢ \n\t%s <:\n\t%s\n\n" *)
    (*     (Typectx.layout Frontend.Undertype.pretty_layout ctx) *)
    (*     (Frontend.Undertype.pretty_layout t1) *)
    (*     (Frontend.Undertype.pretty_layout t2) *)
    (* in *)
    match (t1, t2) with
    | ( UnderTy_base { basename = name1; prop = prop1; _ },
        UnderTy_base { basename = name2; prop = prop2; _ } ) ->
        let prop1, prop2 =
          match (Typectx.in_ctx ctx name1, Typectx.in_ctx ctx name2) with
          | true, true ->
              if String.equal name1 name2 then (prop1, prop2)
              else failwith "subtype bad name"
          | false, true -> (P.subst_id prop1 name1 name2, prop2)
          | _, _ -> (prop1, P.subst_id prop2 name2 name1)
        in
        let pres, res = subtyping_to_query ctx (prop1, prop2) in
        if Autov.check_implies_multi_pre pres res then ()
        else failwith "rejected by the verifier"
    | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
        List.iter (aux ctx) @@ List.combine ts1 ts2
    | ( UnderTy_arrow { argname = x1; argty = t11; retty = t12 },
        UnderTy_arrow { argname = x2; argty = t21; retty = t22 } ) ->
        let t22 = subst_id t22 x2 x1 in
        let () = aux ctx (t21, t11) in
        let () = aux ctx (t12, t22) in
        ()
    | _, _ -> failwith "die: under subtype"
  in
  aux ctx (t1, t2)

let erase_check (underfty, normalty) =
  (* let () = *)
  (*   Printf.printf "|_ %s _| = %s\n" *)
  (*     (Frontend.Undertype.layout underfty) *)
  (*     (Frontend.Type.layout @@ UT.erase underfty) *)
  (* in *)
  Termcheck.check_eq (UT.erase underfty, normalty) "under:bidirect_type_check:"

let rec bidirect_type_infer (ctx : UT.t Typectx.t) (a : NL.term NL.typed) :
    UL.term UL.typed =
  let open NL in
  match a.x with
  | Const _ -> failwith "unimp const type check"
  | Var x ->
      let x = bidirect_type_infer_id ctx { ty = a.ty; x } in
      UL.{ ty = x.ty; x = Var x.x }
  | Tu es ->
      let es = List.map (bidirect_type_infer_id ctx) es in
      let ty = UT.UnderTy_tuple (List.map (fun x -> x.UL.ty) es) in
      UL.{ ty; x = Tu es }
  | Lam (_, _) -> failwith "cannot infer under arrow type"
  | Fix _ -> failwith "unimp"
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
        | _ -> failwith "die:bidirect_type_infer let"
      in
      let lhs =
        List.map (fun (id, idty) ->
            let () = erase_check (idty, id.ty) in
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
      { ty = body.ty; x = Let (lhs, rhs, body) }
  | Ite (_, _, _) -> failwith "cannot infer ite"
  | Match (_, _) -> failwith "cannot infer match"

and bidirect_type_infer_id (ctx : UT.t Typectx.t) (id : NL.id NL.typed) :
    NL.id UL.typed =
  let ty = Typectx.get_ty_with_prim Prim.get_primitive_under_ty ctx id.x in
  let () = erase_check (ty, id.ty) in
  (* TODO: what is the type of variable that is in the context? *)
  let ty =
    UT.(
      match ty with
      | UnderTy_base { basename; normalty; prop } ->
          UnderTy_base
            { basename = id.x; normalty; prop = P.subst_id prop basename id.x }
      | _ -> ty)
  in
  UL.{ ty; x = id.x }

and bidirect_type_check_id (ctx : UT.t Typectx.t) (id : NL.id NL.typed)
    (ty : UT.t) : NL.id UL.typed =
  let id = bidirect_type_infer_id ctx id in
  let () = subtyping_check ctx id.UL.ty ty in
  id

and bidirect_type_check (ctx : UT.t Typectx.t) (x : NL.term NL.typed)
    (ty : UT.t) : UL.term UL.typed =
  let () = Printf.printf "%s\n" (layout_judge ctx (x, ty)) in
  let () = erase_check (ty, x.ty) in
  let open NL in
  match (x.x, ty) with
  | Const _, _ -> failwith "unimp const type check"
  | Var _, _ ->
      let x = bidirect_type_infer ctx x in
      let () = subtyping_check ctx x.ty ty in
      x
  | Tu es, UT.UnderTy_tuple tys ->
      if List.length es != List.length tys then
        failwith "type_check: tuple wrong number"
      else
        let es =
          List.map (fun (e, ty) -> bidirect_type_check_id ctx e ty)
          @@ List.combine es tys
        in
        { ty; x = Tu es }
  | Lam (id, body), UT.(UnderTy_arrow { argname; argty; retty }) ->
      let () = erase_check (argty, id.ty) in
      let retty = UT.subst_id retty argname id.x in
      let ctx' = Typectx.overlap ctx (argty, id.x) in
      let body = bidirect_type_check ctx' body retty in
      { ty; x = Lam ({ ty = argty; x = id.x }, body) }
  | Fix _, _ -> failwith "unimp"
  | App (f, args), ty ->
      let f = bidirect_type_infer_id ctx f in
      let rec check (ctx', args') (args, underftp) =
        match (args, underftp) with
        | [], tp ->
            let () = subtyping_check ctx' tp ty in
            UL.{ ty = tp; x = App ({ ty = underftp; x = f.x }, args') }
        | id :: args, UT.(UnderTy_arrow { argname; argty; retty }) ->
            let id = bidirect_type_infer_id ctx' id in
            let () = subtyping_check ctx' id.ty argty in
            let ctx' = Typectx.overlap ctx' (id.ty, id.x) in
            let retty = UT.subst_id retty argname id.x in
            check (ctx', args' @ [ UL.{ ty = id.ty; x = id.x } ]) (args, retty)
        | _ -> failwith "die:bidirect_type_check app"
      in
      check (ctx, []) (args, f.ty)
  | Let (lhs, rhs, body), ty ->
      let rhs = bidirect_type_infer ctx rhs in
      let lhstys =
        match rhs.ty with
        | UT.UnderTy_tuple ts when List.length ts = List.length lhs -> ts
        | _ when List.length lhs = 1 -> [ rhs.ty ]
        | _ ->
            failwith
            @@ Printf.sprintf "die:bidirect_type_check let (%s) has type (%s)"
                 (List.split_by_comma
                    (fun { ty; x } ->
                      Printf.sprintf "(%s:%s)" x (Frontend.Type.layout ty))
                    lhs)
                 (Frontend.Undertype.layout rhs.ty)
      in
      let lhs =
        List.map (fun (id, idty) ->
            let () = erase_check (idty, id.ty) in
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
  | Ite (id, e1, e2), ty ->
      let id = bidirect_type_infer_id ctx id in
      let true_branch_prop x = Autov.(Prop.(Var { ty = Smtty.Bool; x })) in
      let false_branch_prop x =
        Autov.(Prop.(Not (Var { ty = Smtty.Bool; x })))
      in
      let true_branch_ctx =
        Typectx.overlap ctx
          (UT.base_type_add_conjunction true_branch_prop id.ty, id.x)
      in
      let false_branch_ctx =
        Typectx.overlap ctx
          (UT.base_type_add_conjunction false_branch_prop id.ty, id.x)
      in
      let e1 = bidirect_type_check true_branch_ctx e1 ty in
      let e2 = bidirect_type_check false_branch_ctx e2 ty in
      (* NUTE: underappproximate here *)
      { ty; x = Ite (id, e1, e2) }
  | Match (id, cases), ty ->
      let id = bidirect_type_infer_id ctx id in
      let handle_case { constructor; args; exp } =
        let constructor_ty = Prim.get_primitive_rev_under_ty constructor in
        let constructor_ty = UT.arrow_args_rename args constructor_ty in
        let args, retty = UT.destruct_arrow_tp constructor_ty in
        let retty_prop id =
          UT.(
            match retty with
            | UnderTy_base { basename; prop; _ } -> P.subst_id prop basename id
            | _ -> failwith "bad constructor type")
        in
        let ctx' =
          Typectx.overlaps ctx @@ args
          @ [ (UT.base_type_add_conjunction retty_prop id.ty, id.x) ]
        in
        let exp = bidirect_type_check ctx' exp ty in
        UL.{ constructor; args = List.map snd args; exp }
      in
      let cases = List.map handle_case cases in
      (* NUTE: underappproximate here *)
      { ty; x = Match (id, cases) }
  | _, _ -> failwith "die: undercheck never happen"

let type_check x ty = bidirect_type_check [] x ty

module SNA = Languages.StrucNA
module SOA = Languages.StrucOA

let struc_check l r =
  let open SNA in
  List.iter
    (fun (name', ty) ->
      match List.find_opt (fun { name; _ } -> String.equal name name') l with
      | None -> failwith "does not provide source code"
      | Some { body; _ } ->
          let _ = type_check body ty in
          ())
    r
