open Lang
open Sugar
open Zzdatatype.Datatype
open Normal_id_typing
open Normal_op_typing

type t = Nt.t

let rec bi_typed_term_infer (ctx : t ctx)
    (x : (t option, t option raw_term) typed) : (t, t raw_term) typed =
  match x.ty with
  | None -> bi_term_infer ctx x.x
  | Some ty -> bi_term_check ctx x.x ty

and bi_typed_term_check (ctx : t ctx) (x : (t option, t option raw_term) typed)
    (ty : t) : (t, t raw_term) typed =
  match x.ty with
  | None -> bi_term_check ctx x.x ty
  | Some ty' ->
      let sndty = Nt._type_unify __FILE__ __LINE__ ty' ty in
      bi_term_check ctx x.x sndty

and bi_term_check (ctx : t ctx) (x : t option raw_term) (ty : t) :
    (t, t raw_term) typed =
  match (x, ty) with
  | Err, _ -> Err #: ty
  | Const _, _ | Var _, _ ->
      let x = bi_term_infer ctx x in
      x.x #: (Nt._type_unify __FILE__ __LINE__ x.ty ty)
  | Tu es, Ty_tuple tys ->
      let estys = _safe_combine __FILE__ __LINE__ es tys in
      let es = List.map (fun (e, ty) -> bi_typed_term_check ctx e ty) estys in
      (Tu es) #: ty
  | Lam { lamarg; lambody }, Ty_arrow (t1, _) ->
      (* let _ = Printf.printf "lamarg: %s\n" lamarg.x in *)
      let lamarg = bi_typed_id_check ctx lamarg t1 in
      let ty =
        Nt._type_unify __FILE__ __LINE__ (Ty_arrow (lamarg.ty, Ty_unknown)) ty
      in
      let lambody =
        bi_typed_term_check (add_to_right ctx lamarg) lambody (Nt.get_retty ty)
      in
      (Lam { lamarg; lambody }) #: ty
  | AppOp (op, args), ty ->
      let op' = bi_typed_op_infer ctx op in
      let args' = List.map (bi_typed_term_infer ctx) args in
      let fty =
        Nt._type_unify __FILE__ __LINE__ op'.ty
          (Nt.construct_arr_tp (List.map _get_ty args', ty))
      in
      let argsty, _ = Nt.destruct_arr_tp fty in
      let args =
        List.map (fun (x, ty) -> bi_typed_term_check ctx x ty)
        @@ List.combine args argsty
      in
      let op = bi_typed_op_check ctx op fty in
      (AppOp (op, args)) #: ty
  | App (f, args), ty ->
      let f' = bi_typed_term_infer ctx f in
      let args' = List.map (bi_typed_term_infer ctx) args in
      let fty =
        Nt._type_unify __FILE__ __LINE__
          (Nt.construct_arr_tp (List.map _get_ty args', ty))
          f'.ty
      in
      let argsty, _ = Nt.destruct_arr_tp fty in
      let args =
        List.map (fun (x, ty) -> bi_typed_term_check ctx x ty)
        @@ List.combine args argsty
      in
      let f = bi_typed_term_check ctx f fty in
      (App (f, args)) #: ty
  | Let { if_rec; rhs; lhs; letbody }, ty ->
      let lhs = List.map (__force_typed __FILE__ __LINE__) lhs in
      let rhsty = Nt.mk_tuple (List.map _get_ty lhs) in
      let rhs = bi_typed_term_check ctx rhs rhsty in
      let ctx' = add_to_rights ctx lhs in
      let ctx' =
        if if_rec then _failatwith __FILE__ __LINE__ "todo??"
          (* Typectx.add_to_right ctx ("f", construct_arr_tp (List.map xsty, ty)) *)
        else ctx'
      in
      let letbody = bi_typed_term_check ctx' letbody ty in
      (Let { if_rec; rhs; lhs; letbody }) #: ty
  | Ite (e1, e2, e3), _ ->
      let e1 = bi_typed_term_check ctx e1 Ty_bool in
      let e2 = bi_typed_term_check ctx e2 ty in
      let e3 = bi_typed_term_check ctx e3 ty in
      (Ite (e1, e2, e3)) #: ty
  | Match { match_cases = []; _ }, _ ->
      _failatwith __FILE__ __LINE__
        "bi_term_infer: pattern matching branch is empty"
  | Match { matched; match_cases }, _ ->
      let matched = bi_typed_term_infer ctx matched in
      let handle_case = function
        | Matchcase { constructor; args; exp } ->
            let constructor_ty =
              Nt.construct_arr_tp
                (List.map (fun _ -> Nt.Ty_unknown) args, matched.ty)
            in
            let constructor =
              bi_typed_id_check ctx constructor constructor_ty
            in
            let argsty, _ = Nt.destruct_arr_tp constructor.ty in
            let args =
              List.map (fun (x, ty) -> x.x #: ty)
              @@ _safe_combine __FILE__ __LINE__ args argsty
            in
            let ctx' = add_to_rights ctx args in
            let exp = bi_typed_term_check ctx' exp ty in
            Matchcase { constructor; args; exp }
      in
      let match_cases = List.map handle_case match_cases in
      (Match { matched; match_cases }) #: ty
  | e, ty ->
      _failatwith __FILE__ __LINE__
        (spf "bi_term_check: inconsistent term (%s) and type (%s)"
           (Rawlang.layout_raw_term e)
           (Nt.layout ty))

and bi_term_infer (ctx : t ctx) (x : t option raw_term) : (t, t raw_term) typed
    =
  match x with
  | Err ->
      failwith
        "Cannot infer the type of the exception, should provide the return type"
  | Const c -> (Const c) #: (Normal_constant_typing.infer_constant c)
  | Var id ->
      (* let _ = Printf.printf "id: %s\n" id.x in *)
      let id = bi_typed_id_infer ctx id in
      (Var id) #: id.ty
  | Tu es ->
      let es = List.map (bi_typed_term_infer ctx) es in
      let ty = Nt.mk_tuple (List.map _get_ty es) in
      (Tu es) #: ty
  | Lam { lamarg; lambody } ->
      let lamarg = __force_typed __FILE__ __LINE__ lamarg in
      let lambody = bi_typed_term_infer (add_to_right ctx lamarg) lambody in
      let ty = Nt.construct_arr_tp ([ lamarg.ty ], lambody.ty) in
      (Lam { lamarg; lambody }) #: ty
  | AppOp (op, args) ->
      let args = List.map (bi_typed_term_infer ctx) args in
      let op =
        bi_typed_op_check ctx op
          (Nt.construct_arr_tp (List.map _get_ty args, Ty_unknown))
      in
      let _, ty = Nt.destruct_arr_tp op.ty in
      (AppOp (op, args)) #: ty
  | App (f, args) ->
      let f = bi_typed_term_infer ctx f in
      let argsty, ty = Nt.destruct_arr_tp f.ty in
      let args =
        List.map (fun (x, ty) -> bi_typed_term_check ctx x ty)
        @@ _safe_combine __FILE__ __LINE__ args argsty
      in
      (App (f, args)) #: ty
  | Let { if_rec = true; _ } ->
      _failatwith __FILE__ __LINE__
        "cannot infer ret type of recursive function"
  | Let { if_rec; rhs; lhs; letbody } ->
      let lhs = List.map (__force_typed __FILE__ __LINE__) lhs in
      let rhsty = Nt.mk_tuple (List.map _get_ty lhs) in
      let rhs = bi_typed_term_check ctx rhs rhsty in
      let ctx' = add_to_rights ctx lhs in
      let ctx' =
        if if_rec then _failatwith __FILE__ __LINE__ "todo??"
          (* Typectx.add_to_right ctx ("f", construct_arr_tp (List.map xsty, ty)) *)
        else ctx'
      in
      let letbody = bi_typed_term_infer ctx' letbody in
      (Let { if_rec; rhs; lhs; letbody }) #: letbody.ty
  | Ite (e1, e2, e3) ->
      let e1 = bi_typed_term_check ctx e1 Ty_bool in
      let e2 = bi_typed_term_infer ctx e2 in
      let e3 = bi_typed_term_check ctx e3 e2.ty in
      (Ite (e1, e2, e3)) #: e2.ty
  | Match { matched; match_cases } ->
      let matched = bi_typed_term_infer ctx matched in
      let handle_case = function
        | Matchcase { constructor; args; exp } ->
            let constructor_ty =
              Nt.construct_arr_tp
                (List.map (fun _ -> Nt.Ty_unknown) args, matched.ty)
            in
            let constructor =
              bi_typed_id_check ctx constructor constructor_ty
            in
            let argsty, _ = Nt.destruct_arr_tp constructor.ty in
            let args =
              List.map (fun (x, ty) -> x.x #: ty)
              @@ _safe_combine __FILE__ __LINE__ args argsty
            in
            let ctx' = add_to_rights ctx args in
            let exp = bi_typed_term_infer ctx' exp in
            Matchcase { constructor; args; exp }
      in
      let match_cases = List.map handle_case match_cases in
      let ty =
        match match_cases with
        | [] ->
            _failatwith __FILE__ __LINE__
              "bi_term_infer: pattern matching branch is empty"
        | Matchcase { exp; _ } :: match_cases ->
            let ty = exp.ty in
            if
              List.for_all
                (function Matchcase { exp; _ } -> Nt.eq exp.ty ty)
                match_cases
            then ty
            else
              _failatwith __FILE__ __LINE__
                "bi_term_infer: pattern matching branchs have different types"
      in
      (Match { matched; match_cases }) #: ty

let typed_term_infer = bi_typed_term_infer
let typed_term_check = bi_typed_term_check
