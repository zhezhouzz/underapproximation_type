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
  | NotADt -> _failatwith __FILE__ __LINE__ "die"

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

(* let fail_as b str = if b then () else failwith str *)

(* let check_eq (t1, t2) str = *)
(*   fail_as (eq t1 t2) *)
(*     (spf "%stype %s is not equal to type %s" str (layout t1) (layout t2)) *)

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
      let ty = _check_equality __FILE__ __LINE__ eq ty ty' in
      type_check ctx x.x ty

and type_check (ctx : t Typectx.t) (x : Exp.term) (ty : t) :
    Exp.term Exp.opttyped =
  let open Exp in
  match (x, ty) with
  | Const _, _ | Var _, _ | Op (_, _), _ ->
      let x, ty' = type_infer ctx x in
      let _ = _check_equality __FILE__ __LINE__ eq ty ty' in
      x
  | Tu es, Ty_tuple tys ->
      let estys = _safe_combine __FILE__ __LINE__ es tys in
      let es = List.map (fun (e, ty) -> bidirect_type_check ctx e ty) estys in
      { ty = Some ty; x = Tu es }
  | Lam (idty, id, body), Ty_arrow (t1, t2) ->
      let idty = _check_equality __FILE__ __LINE__ eq idty t1 in
      let ctx' = Typectx.add_to_right ctx (idty, id) in
      let body = bidirect_type_check ctx' body t2 in
      { ty = Some ty; x = Lam (idty, id, body) }
  | App (f, args), ty ->
      let f, fty = bidirect_type_infer ctx f in
      let argsty, bodyty = destruct_arrow_tp fty in
      let ty = _check_equality __FILE__ __LINE__ eq bodyty ty in
      let argsargsty = _safe_combine __FILE__ __LINE__ args argsty in
      let args =
        List.map (fun (e, ty) -> bidirect_type_check ctx e ty) argsargsty
      in
      { ty = Some ty; x = App (f, args) }
  | Let (if_rec, args, rhs, body), ty ->
      let xsty = List.map fst args in
      let rhsty =
        match xsty with
        | [] ->
            _failatwith __FILE__ __LINE__ "type_infer: let binding lhs is empty"
        | [ tp ] -> tp
        | l -> Ty_tuple l
      in
      let rhs = bidirect_type_check ctx rhs rhsty in
      let ctx' = List.fold_left Typectx.add_to_right ctx args in
      let ctx' =
        if if_rec then
          Typectx.add_to_right ctx (construct_arrow_tp (xsty, ty), "f")
        else ctx'
      in
      let body = bidirect_type_check ctx' body ty in
      { ty = Some ty; x = Let (if_rec, args, rhs, body) }
  | Ite (e1, e2, e3), ty ->
      let e1 = bidirect_type_check ctx e1 Ty_bool in
      let e2 = bidirect_type_check ctx e2 ty in
      let e3 = bidirect_type_check ctx e3 ty in
      { ty = Some ty; x = Ite (e1, e2, e3) }
  | Match (_, []), _ ->
      _failatwith __FILE__ __LINE__
        "type_infer: pattern matching branch is empty"
  | Match (e, cases), ty ->
      let e, ety = bidirect_type_infer ctx e in
      let handle_case { constructor; args; exp } =
        let constructor_ty =
          Prim.get_primitive_dt_rev_normal_ty (constructor.x, ety)
        in
        let constructor = { ty = Some constructor_ty; x = constructor.x } in
        let argsty, _ = destruct_arrow_tp constructor_ty in
        let ctx' =
          List.fold_left Typectx.add_to_right ctx (List.combine argsty args)
        in
        let exp = bidirect_type_check ctx' exp ty in
        let case = { constructor; args; exp } in
        case
      in
      { ty = Some ty; x = Match (e, List.map handle_case cases) }
  | e, ty ->
      _failatwith __FILE__ __LINE__
        (spf "type_check: inconsistent term (%s) and type (%s)"
           (Frontend.Expr.layout { ty = None; x = e })
           (Frontend.Type.layout ty))

and type_infer (ctx : t Typectx.t) (x : Exp.term) : Exp.term Exp.opttyped * t =
  let open Exp in
  match x with
  | Const c ->
      let ty = infer_value c in
      ({ ty = Some ty; x }, ty)
  | Var id ->
      let ty =
        try Typectx.get_ty ctx id
        with _ -> Prim.get_primitive_normal_ty (External id)
      in
      ({ ty = Some ty; x }, ty)
  | Tu es ->
      let es, esty = List.split @@ List.map (bidirect_type_infer ctx) es in
      let ty = Ty_tuple esty in
      ({ ty = Some ty; x = Tu es }, ty)
  | Lam (idty, id, body) ->
      let ctx' = Typectx.add_to_right ctx (idty, id) in
      let body, bodyty = bidirect_type_infer ctx' body in
      let ty = Ty_arrow (idty, bodyty) in
      ({ ty = Some ty; x = Lam (idty, id, body) }, ty)
  | Op (op, args) ->
      let args, argsty =
        List.split @@ List.map (bidirect_type_infer ctx) args
      in
      let ty = Opcheck.check (op, argsty) in
      ({ ty = Some ty; x = Op (op, args) }, ty)
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
            _failatwith __FILE__ __LINE__
              "type_infer: App, not a function type or wrong args number"
      in
      let ty, args = aux (fty, args) in
      ({ ty = Some ty; x = App (f, args) }, ty)
  | Let (true, _, _, _) ->
      _failatwith __FILE__ __LINE__
        "cannot infer ret type of recursive function"
  | Let (if_rec, args, rhs, body) ->
      (* let () = Printf.printf "let!!!\n" in *)
      (* let () = *)
      (*   if if_rec then *)
      (*     Printf.printf "rec::: %s\n" @@ Frontend.Expr.layout { ty = None; x } *)
      (*   else () *)
      (* in *)
      let rhsty =
        match List.map fst args with
        | [] ->
            _failatwith __FILE__ __LINE__ "type_infer: let binding lhs is empty"
        | [ tp ] -> tp
        | l -> Ty_tuple l
      in
      let rhs = bidirect_type_check ctx rhs rhsty in
      let ctx' = List.fold_left Typectx.add_to_right ctx args in
      let body, bodyty = bidirect_type_infer ctx' body in
      let ty = bodyty in
      ({ ty = Some ty; x = Let (if_rec, args, rhs, body) }, ty)
  | Ite (e1, e2, e3) ->
      let e1 = bidirect_type_check ctx e1 Ty_bool in
      let e2, ty = bidirect_type_infer ctx e2 in
      let e3 = bidirect_type_check ctx e3 ty in
      ({ ty = Some ty; x = Ite (e1, e2, e3) }, ty)
  | Match (_, []) ->
      _failatwith __FILE__ __LINE__
        "type_infer: pattern matching branch is empty"
  | Match (e, cases) ->
      let e, ety = bidirect_type_infer ctx e in
      let handle_case { constructor; args; exp } =
        let constructor_ty =
          Prim.get_primitive_dt_rev_normal_ty (constructor.x, ety)
        in
        let constructor = { ty = Some constructor_ty; x = constructor.x } in
        let argsty, _ = destruct_arrow_tp constructor_ty in
        let ctx' =
          List.fold_left Typectx.add_to_right ctx (List.combine argsty args)
        in
        let exp, expty = bidirect_type_infer ctx' exp in
        let case = { constructor; args; exp } in
        (case, expty)
      in
      let cases, exptys = List.split @@ List.map handle_case cases in
      let ty =
        match exptys with
        | [] -> _failatwith __FILE__ __LINE__ "die"
        | ty :: t ->
            List.fold_left
              (fun ty ty' -> _check_equality __FILE__ __LINE__ eq ty ty')
              ty t
      in
      ({ ty = Some ty; x = Match (e, cases) }, ty)

let check e = fst @@ bidirect_type_infer Typectx.empty e

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
      | true, None ->
          _failatwith __FILE__ __LINE__
            "cannot infer ret type of recursive function"
      | true, Some ty ->
          let body =
            bidirect_type_check Typectx.(add_to_right empty (ty, name)) body ty
          in
          { if_rec; name; body })
    l
