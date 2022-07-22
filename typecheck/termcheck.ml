module Exp = Languages.Termlang
module V = Languages.Value
module Type = Languages.Normalty
open Zzdatatype.Datatype
open Type
open Sugar
open Abstraction

let layout = Frontend.Type.layout

let rec infer_value (c : Value.t) =
  match c with
  | U -> Ty_unit
  | I _ -> Ty_int
  | B _ -> Ty_bool
  | IL _ -> Ty_list Ty_int
  | IT _ -> Ty_tree Ty_int
  | Tu l -> Ty_tuple (List.map infer_value l)
  | NotADt -> failwith "infer_value"

let rec check_against_value (c : Value.t) ty =
  let open Value in
  match (c, ty) with
  | U, Ty_unit -> true
  | I _, Ty_int -> true
  | B _, Ty_bool -> true
  | IL _, Ty_list Ty_int -> true
  | IT _, Ty_tree Ty_int -> true
  | Tu l, Ty_tuple tys ->
      if List.length l != List.length tys then false
      else
        List.for_all (fun (c, ty) -> check_against_value c ty)
        @@ List.combine l tys
  | _, _ -> false

let fail_as b str = if b then () else failwith str

let check_eq (t1, t2) str =
  fail_as
    (eq (t1, t2))
    (spf "%stype %s is not equal to type %s" str (layout t1) (layout t2))

let rec bidirect_type_infer (ctx : t Typectx.t) (x : Exp.term Exp.opttyped) :
    Exp.term Exp.opttyped * t =
  match x.ty with
  | None -> type_infer ctx x.x
  | Some ty -> (type_check ctx x.x ty, ty)

and bidirect_type_check (ctx : t Typectx.t) (x : Exp.term Exp.opttyped) (ty : t)
    : Exp.term Exp.opttyped =
  match x.ty with
  | None -> type_check ctx x.x ty
  | Some ty' ->
      let () = check_eq (ty, ty') "bidirect_type_check:" in
      type_check ctx x.x ty

and type_check (ctx : t Typectx.t) (x : Exp.term) (ty : t) :
    Exp.term Exp.opttyped =
  let open Exp in
  match (x, ty) with
  | Const c, _ ->
      let ty' = infer_value c in
      let () = check_eq (ty, ty') "type_check:const:" in
      { ty = Some ty; x }
  | Var id, _ ->
      let ty' = Typectx.get_ty_with_prim Prim.get_primitive_normal_ty ctx id in
      let () = check_eq (ty, ty') "type_check:var:" in
      { ty = Some ty; x }
  | Tu es, Ty_tuple tys ->
      if List.length es != List.length tys then
        failwith "type_check: tuple wrong number"
      else
        let es =
          List.map (fun (e, ty) -> bidirect_type_check ctx e ty)
          @@ List.combine es tys
        in
        { ty = Some ty; x = Tu es }
  | Lam (idty, id, body), Ty_arrow (t1, t2) ->
      let () = check_eq (idty, t1) "type_check:Lam:" in
      let ctx' = Typectx.overlap ctx (idty, id) in
      let body = bidirect_type_check ctx' body t2 in
      { ty = Some ty; x = Lam (idty, id, body) }
  | App (f, args), ty ->
      let f, fty = bidirect_type_infer ctx f in
      let argsty, bodyty = destruct_arrow_tp fty in
      let () = check_eq (bodyty, ty) "type_check:App:" in
      if List.length args != List.length argsty then
        failwith "type_check:App: wrong args number"
      else
        let args =
          List.map (fun (e, ty) -> bidirect_type_check ctx e ty)
          @@ List.combine args argsty
        in
        { ty = Some ty; x = App (f, args) }
  | Let (if_rec, args, rhs, body), ty ->
      let () =
        if if_rec then
          Printf.printf "rec::: %s\n" @@ Frontend.Expr.layout { ty = None; x }
        else ()
      in
      let xsty = List.map fst args in
      let rhsty =
        match xsty with
        | [] -> failwith "type_infer: let binding lhs is empty"
        | [ tp ] -> tp
        | l -> Ty_tuple l
      in
      let rhs = bidirect_type_check ctx rhs rhsty in
      let ctx' = List.fold_left Typectx.overlap ctx args in
      let ctx' =
        if if_rec then Typectx.overlap ctx (construct_arrow_tp (xsty, ty), "f")
        else ctx'
      in
      let body = bidirect_type_check ctx' body ty in
      { ty = Some ty; x = Let (if_rec, args, rhs, body) }
  | Ite (e1, e2, e3), ty ->
      let e1 = bidirect_type_check ctx e1 Ty_bool in
      let e2 = bidirect_type_check ctx e2 ty in
      let e3 = bidirect_type_check ctx e3 ty in
      { ty = Some ty; x = Ite (e1, e2, e3) }
  | Match (e, cases), ty -> (
      match cases with
      | [] -> failwith "type_infer: pattern matching branch is empty"
      | { constructor; args; exp } :: cases ->
          let argsty, bodyty =
            destruct_arrow_tp @@ Prim.get_primitive_normal_ty constructor
          in
          let ctx' =
            List.fold_left Typectx.overlap ctx (List.combine argsty args)
          in
          let exp = bidirect_type_check ctx' exp ty in
          let case = { constructor; args; exp } in
          let ety = bodyty in
          let cases =
            List.map
              (fun { constructor; args; exp } ->
                let argsty, bodyty =
                  destruct_arrow_tp @@ Prim.get_primitive_normal_ty constructor
                in
                let () = check_eq (ety, bodyty) "type_infer:Match" in
                let ctx' =
                  List.fold_left Typectx.overlap ctx (List.combine argsty args)
                in
                let exp = bidirect_type_check ctx' exp ty in
                { constructor; args; exp })
              cases
          in
          let e = bidirect_type_check ctx e ety in
          { ty = Some ty; x = Match (e, case :: cases) })
  | _, _ -> failwith "type_check: inconsistent term and type"

and type_infer (ctx : t Typectx.t) (x : Exp.term) : Exp.term Exp.opttyped * t =
  let open Exp in
  match x with
  | Const c ->
      let ty = infer_value c in
      ({ ty = Some ty; x }, ty)
  | Var id ->
      let ty = Typectx.get_ty_with_prim Prim.get_primitive_normal_ty ctx id in
      ({ ty = Some ty; x }, ty)
  | Tu es ->
      let es, esty = List.split @@ List.map (bidirect_type_infer ctx) es in
      let ty = Ty_tuple esty in
      ({ ty = Some ty; x = Tu es }, ty)
  | Lam (idty, id, body) ->
      let ctx' = Typectx.overlap ctx (idty, id) in
      let body, bodyty = bidirect_type_infer ctx' body in
      let ty = Ty_arrow (idty, bodyty) in
      ({ ty = Some ty; x = Lam (idty, id, body) }, ty)
  | App (f, args) ->
      let f, fty = bidirect_type_infer ctx f in
      let rec aux (fty, args) =
        match (fty, args) with
        | _, [] -> (fty, [])
        | Ty_arrow (t1, t2), arg :: args ->
            let arg = bidirect_type_check ctx arg t1 in
            let bodytp, args = aux (t2, args) in
            (bodytp, arg :: args)
        | _ ->
            failwith "type_infer: App, not a function type or wrong args number"
      in
      let ty, args = aux (fty, args) in
      ({ ty = Some ty; x = App (f, args) }, ty)
  | Let (true, _, _, _) ->
      failwith "cannot infer ret type of recursive function"
  | Let (if_rec, args, rhs, body) ->
      (* let () = Printf.printf "let!!!\n" in *)
      (* let () = *)
      (*   if if_rec then *)
      (*     Printf.printf "rec::: %s\n" @@ Frontend.Expr.layout { ty = None; x } *)
      (*   else () *)
      (* in *)
      let rhsty =
        match List.map fst args with
        | [] -> failwith "type_infer: let binding lhs is empty"
        | [ tp ] -> tp
        | l -> Ty_tuple l
      in
      let rhs = bidirect_type_check ctx rhs rhsty in
      let ctx' = List.fold_left Typectx.overlap ctx args in
      let body, bodyty = bidirect_type_infer ctx' body in
      let ty = bodyty in
      ({ ty = Some ty; x = Let (if_rec, args, rhs, body) }, ty)
  | Ite (e1, e2, e3) ->
      let e1 = bidirect_type_check ctx e1 Ty_bool in
      let e2, ty = bidirect_type_infer ctx e2 in
      let e3 = bidirect_type_check ctx e3 ty in
      ({ ty = Some ty; x = Ite (e1, e2, e3) }, ty)
  | Match (e, cases) -> (
      match cases with
      | [] -> failwith "type_infer: pattern matching branch is empty"
      | { constructor; args; exp } :: cases ->
          let argsty, bodyty =
            destruct_arrow_tp @@ Prim.get_primitive_normal_ty constructor
          in
          let ctx' =
            List.fold_left Typectx.overlap ctx (List.combine argsty args)
          in
          let exp, ty = bidirect_type_infer ctx' exp in
          let case = { constructor; args; exp } in
          let ety = bodyty in
          let cases =
            List.map
              (fun { constructor; args; exp } ->
                let argsty, bodyty =
                  destruct_arrow_tp @@ Prim.get_primitive_normal_ty constructor
                in
                let () = check_eq (ety, bodyty) "type_infer:Match" in
                let ctx' =
                  List.fold_left Typectx.overlap ctx (List.combine argsty args)
                in
                let exp = bidirect_type_check ctx' exp ty in
                { constructor; args; exp })
              cases
          in
          let e = bidirect_type_check ctx e ety in
          ({ ty = Some ty; x = Match (e, case :: cases) }, ty))

let check e = fst @@ bidirect_type_infer [] e

module LS = Languages.Struc

let struc_check l =
  let open LS in
  List.map
    (fun { if_rec; name; body } ->
      let open Exp in
      let rec get_fty e =
        match e.x with
        | Lam (ty, _, body) ->
            Sugar.(
              let* bty = get_fty body in
              Some (Ty_arrow (ty, bty)))
        | _ -> e.ty
      in
      match (if_rec, get_fty body) with
      | false, _ -> { if_rec; name; body = check body }
      | true, None -> failwith "cannot infer ret type of recursive function"
      | true, Some ty ->
          let body = bidirect_type_check [ (name, ty) ] body ty in
          { if_rec; name; body })
    l
