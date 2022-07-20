module NL = Languages.NormalAnormal
module OL = Languages.OverAnormal
module NT = Languages.Normalty
module OT = Languages.Overty
open Zzdatatype.Datatype
open Sugar

let subtyping_check (ctx : OT.t Typectx.t) (t : OT.t) (t' : OT.t) =
  failwith "unimp"

let erase_check (overfty, normalty) =
  Termcheck.check_eq (OT.erase overfty, normalty) "over:bidirect_type_check:"

let rec bidirect_type_infer (ctx : OT.t Typectx.t) (x : NL.term NL.typed) :
    OL.term OL.typed * OT.t =
  failwith "unimp"
(* let idty = OL.{ ty = Typectx.get_ty ctx id; x = Var id } in *)

and bidirect_type_infer_id (ctx : OT.t Typectx.t) (id : NL.id NL.typed) :
    NL.id OL.typed * OT.t =
  let ty = Typectx.get_ty ctx id.x in
  let () = erase_check (ty, id.ty) in
  (OL.{ ty = Typectx.get_ty ctx id.x; x = id.x }, ty)

and bidirect_type_check_id (ctx : OT.t Typectx.t) (id : NL.id NL.typed)
    (ty : OT.t) : NL.id OL.typed =
  let id, idty = bidirect_type_infer_id ctx id in
  let () = subtyping_check ctx idty ty in
  id

and bidirect_type_check (ctx : OT.t Typectx.t) (x : NL.term NL.typed)
    (ty : OT.t) : OL.term OL.typed =
  let () = erase_check (ty, x.ty) in
  let open NL in
  match (x.x, ty) with
  | Const _, _ -> failwith "unimp const type check"
  | Var _, _ ->
      let x, xty = bidirect_type_infer ctx x in
      let () = subtyping_check ctx xty ty in
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
      let overftp = Typectx.get_ty ctx f.x in
      let () = erase_check (overftp, f.ty) in
      let rec check (ctx', args') (args, overftp) =
        match (args, overftp) with
        | [], tp ->
            let () = subtyping_check ctx' tp ty in
            OL.{ ty = tp; x = App ({ ty = overftp; x = f.x }, args') }
        | id :: args, OT.(OverTy_arrow { argname; argty; retty }) ->
            let idty = Typectx.get_ty ctx' id.x in
            let () = erase_check (idty, id.ty) in
            let () = subtyping_check ctx' idty argty in
            let ctx' = Typectx.overlap ctx' (idty, id.x) in
            let retty = OT.subst_id retty argname id.x in
            check (ctx', args' @ [ OL.{ ty = idty; x = id.x } ]) (args, retty)
        | _ -> failwith "die:bidirect_type_check"
      in
      check (ctx, []) (args, overftp)
