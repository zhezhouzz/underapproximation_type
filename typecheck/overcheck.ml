module NL = Languages.NormalAnormal
module OL = Languages.OverAnormal
module NT = Languages.Normalty
module OT = Languages.Overty
open Zzdatatype.Datatype
open Abstraction

let layout_judge = Frontend.Typectx.pretty_layout_over_judge Trans.nan_to_term
let layout_subtyping = Frontend.Typectx.pretty_layout_under_subtyping

let subtyping_to_query ctx (prop1, prop2) =
  let fv1 = Autov.prop_fv prop1 in
  let fv2 = Autov.prop_fv prop2 in
  let fv = fv1 @ fv2 in
  let ctx =
    List.filter_map
      (fun (x, ty) ->
        OT.(
          match ty with
          | OverTy_base { basename; prop; _ } ->
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
  (* let () = *)
  (*   Printf.printf "SMT check:\n\twith ctx: %s\n\t(%s) => (%s)\n\n" *)
  (*     (List.split_by " ∧ " Autov.pretty_layout_prop pre) *)
  (*     (Autov.pretty_layout_prop prop1) *)
  (*     (Autov.pretty_layout_prop prop2) *)
  (* in *)
  (pre @ [ prop1 ], prop2)

let subtyping_check (ctx : OT.t Typectx.t) (t1 : OT.t) (t2 : OT.t) =
  let open OT in
  let rec aux ctx (t1, t2) =
    (* let () = *)
    (*   Printf.printf "%s ⊢ \n\t%s <:\n\t%s\n\n" *)
    (*     (Frontend.Overtype.pretty_layout_typectx ctx) *)
    (*     (Frontend.Overtype.pretty_layout t1) *)
    (*     (Frontend.Overtype.pretty_layout t2) *)
    (* in *)
    match (t1, t2) with
    | ( OverTy_base { basename = name1; prop = prop1; _ },
        OverTy_base { basename = name2; prop = prop2; _ } ) ->
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
    | OverTy_tuple ts1, OverTy_tuple ts2 ->
        List.iter (aux ctx) @@ List.combine ts1 ts2
    | ( OverTy_arrow { argname = x1; argty = t11; retty = t12 },
        OverTy_arrow { argname = x2; argty = t21; retty = t22 } ) ->
        let t22 = subst_id t22 x2 x1 in
        let () = aux ctx (t21, t11) in
        let () = aux ctx (t12, t22) in
        ()
    | _, _ -> failwith "die: over subtype"
  in
  aux ctx (t1, t2)

let erase_check (overfty, normalty) =
  (* let () = *)
  (*   Printf.printf "|_ %s _| = %s\n" *)
  (*     (Frontend.Overtype.layout overfty) *)
  (*     (Frontend.Type.layout @@ OT.erase overfty) *)
  (* in *)
  Termcheck.check_eq (OT.erase overfty, normalty) "over:bidirect_type_check:"

let rec bidirect_type_infer (ctx : OT.t Typectx.t) (a : NL.term NL.typed) :
    OL.term OL.typed =
  let open NL in
  match a.x with
  | Const _ -> failwith "unimp const type check"
  | Var x ->
      let x = bidirect_type_infer_id ctx { ty = a.ty; x } in
      OL.{ ty = x.ty; x = Var x.x }
  | Tu es ->
      let es = List.map (bidirect_type_infer_id ctx) es in
      let ty = OT.OverTy_tuple (List.map (fun x -> x.OL.ty) es) in
      OL.{ ty; x = Tu es }
  | Lam (_, _) -> failwith "cannot infer over arrow type"
  | Fix _ -> failwith "unimp"
  | App (f, args) ->
      let f = bidirect_type_infer_id ctx f in
      let fty' = OT.arrow_args_rename (List.map (fun x -> x.x) args) f.ty in
      let argsty', retty' = OT.destruct_arrow_tp fty' in
      let args =
        List.map (fun ((argty, _), arg) -> bidirect_type_check_id ctx arg argty)
        @@ List.combine argsty' args
      in
      OL.{ ty = retty'; x = App (f, args) }
  | Let (lhs, rhs, body) ->
      let rhs = bidirect_type_infer ctx rhs in
      let lhstys =
        match rhs.ty with
        | OT.OverTy_tuple ts when List.length ts = List.length lhs -> ts
        | _ -> failwith "die:bidirect_type_infer let"
      in
      let lhs =
        List.map (fun (id, idty) ->
            let () = erase_check (idty, id.ty) in
            OL.{ ty = idty; x = id.x })
        @@ List.combine lhs lhstys
      in
      let ctx' =
        List.fold_left
          (fun ctx' id ->
            let ctx' = Typectx.overlap ctx' OL.(id.ty, id.x) in
            ctx')
          ctx lhs
      in
      let body = bidirect_type_infer ctx' body in
      { ty = body.ty; x = Let (lhs, rhs, body) }
  | Ite (_, _, _) -> failwith "cannot infer ite"
  | Match (_, _) -> failwith "cannot infer match"

and bidirect_type_infer_id (ctx : OT.t Typectx.t) (id : NL.id NL.typed) :
    NL.id OL.typed =
  let ty = Typectx.get_ty_with_prim Prim.get_primitive_over_ty ctx id.x in
  let () = erase_check (ty, id.ty) in
  (* TODO: what is the type of variable that is in the context? *)
  let ty =
    OT.(
      match ty with
      | OverTy_base { basename; normalty; prop } ->
          OverTy_base
            { basename = id.x; normalty; prop = P.subst_id prop basename id.x }
      | _ -> ty)
  in
  OL.{ ty; x = id.x }

and bidirect_type_check_id (ctx : OT.t Typectx.t) (id : NL.id NL.typed)
    (ty : OT.t) : NL.id OL.typed =
  let id = bidirect_type_infer_id ctx id in
  let () = subtyping_check ctx id.OL.ty ty in
  id

and bidirect_type_check (ctx : OT.t Typectx.t) (x : NL.term NL.typed)
    (ty : OT.t) : OL.term OL.typed =
  let () = Printf.printf "%s\n" (layout_judge ctx (x, ty)) in
  let () = erase_check (ty, x.ty) in
  let open NL in
  match (x.x, ty) with
  | Const _, _ -> failwith "unimp const type check"
  | Var _, _ ->
      let x = bidirect_type_infer ctx x in
      let () = subtyping_check ctx x.ty ty in
      x
  | Tu es, OT.OverTy_tuple tys ->
      if List.length es != List.length tys then
        failwith "type_check: tuple wrong number"
      else
        let es =
          List.map (fun (e, ty) -> bidirect_type_check_id ctx e ty)
          @@ List.combine es tys
        in
        { ty; x = Tu es }
  | Lam (id, body), OT.(OverTy_arrow { argname; argty; retty }) ->
      let () = erase_check (argty, id.ty) in
      let retty = OT.subst_id retty argname id.x in
      let ctx' = Typectx.overlap ctx (argty, id.x) in
      let body = bidirect_type_check ctx' body retty in
      { ty; x = Lam ({ ty = argty; x = id.x }, body) }
  | Fix _, _ -> failwith "unimp"
  | App (f, args), ty ->
      let f = bidirect_type_infer_id ctx f in
      let rec check (ctx', args') (args, overftp) =
        match (args, overftp) with
        | [], tp ->
            let () = subtyping_check ctx' tp ty in
            OL.{ ty = tp; x = App ({ ty = overftp; x = f.x }, args') }
        | id :: args, OT.(OverTy_arrow { argname; argty; retty }) ->
            let id = bidirect_type_infer_id ctx' id in
            let () = subtyping_check ctx' id.ty argty in
            let ctx' = Typectx.overlap ctx' (id.ty, id.x) in
            let retty = OT.subst_id retty argname id.x in
            check (ctx', args' @ [ OL.{ ty = id.ty; x = id.x } ]) (args, retty)
        | _ -> failwith "die:bidirect_type_check app"
      in
      check (ctx, []) (args, f.ty)
  | Let (lhs, rhs, body), ty ->
      let rhs = bidirect_type_infer ctx rhs in
      let lhstys =
        match rhs.ty with
        | OT.OverTy_tuple ts when List.length ts = List.length lhs -> ts
        | _ when List.length lhs = 1 -> [ rhs.ty ]
        | _ ->
            failwith
            @@ Printf.sprintf "die:bidirect_type_check let (%s) has type (%s)"
                 (List.split_by_comma
                    (fun { ty; x } ->
                      Printf.sprintf "(%s:%s)" x (Frontend.Type.layout ty))
                    lhs)
                 (Frontend.Overtype.layout rhs.ty)
      in
      let lhs =
        List.map (fun (id, idty) ->
            let () = erase_check (idty, id.ty) in
            OL.{ ty = idty; x = id.x })
        @@ List.combine lhs lhstys
      in
      let ctx' =
        List.fold_left
          (fun ctx' id ->
            let ctx' = Typectx.overlap ctx' OL.(id.ty, id.x) in
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
          (OT.base_type_add_conjunction true_branch_prop id.ty, id.x)
      in
      let false_branch_ctx =
        Typectx.overlap ctx
          (OT.base_type_add_conjunction false_branch_prop id.ty, id.x)
      in
      let e1 = bidirect_type_check true_branch_ctx e1 ty in
      let e2 = bidirect_type_check false_branch_ctx e2 ty in
      (* NOTE: overappproximate here *)
      { ty; x = Ite (id, e1, e2) }
  | Match (id, cases), ty ->
      let id = bidirect_type_infer_id ctx id in
      let handle_case { constructor; args; exp } =
        let constructor_ty = Prim.get_primitive_over_ty constructor in
        let constructor_ty = OT.arrow_args_rename args constructor_ty in
        let args, retty = OT.destruct_arrow_tp constructor_ty in
        let retty_prop id =
          OT.(
            match retty with
            | OverTy_base { basename; prop; _ } -> P.subst_id prop basename id
            | _ -> failwith "bad constructor type")
        in
        let ctx' =
          Typectx.overlaps ctx @@ args
          @ [ (OT.base_type_add_conjunction retty_prop id.ty, id.x) ]
        in
        let exp = bidirect_type_check ctx' exp ty in
        OL.{ constructor; args = List.map snd args; exp }
      in
      let cases = List.map handle_case cases in
      (* NOTE: overappproximate here *)
      { ty; x = Match (id, cases) }
  | _, _ -> failwith "die: overcheck never happen"

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
