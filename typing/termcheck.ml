open Language
open FrontendTyped
open Zzdatatype.Datatype
open Sugar
open Subtyping

type t = Nt.t

let layout_ty = Nt.layout
let _rec_arg : t prop option ref = ref None
let init_rec_arg x = _rec_arg := Some x

let apply_rec_arg arg =
  match !_rec_arg with
  | Some p ->
      let arg = (AVar arg) #: arg.ty in
      let param = (AVar default_v #: Nt.int_ty) #: Nt.int_ty in
      let phi = apply_pi_prop (apply_pi_prop p arg) param in
      Cty { nty = Nt.int_ty; phi }
  | None -> _failatwith __FILE__ __LINE__ "die"

let _cur_rec_func_name : (string * t cty) option ref = ref None
let init_cur_rec_func_name (fname, cty) = _cur_rec_func_name := Some (fname, cty)

exception RecArgCheckFailure

let get_cur_rec_func_name () =
  match !_cur_rec_func_name with
  | Some (fname, cty) -> Some (fname, RtyBase { ou = false; cty })
  | None -> None

let _warinning_subtyping_error file line (rty1, rty2) =
  Env.show_debug_typing @@ fun _ ->
  Pp.printf "@{<bold>Type Error at [%s::%i]:@} %s <: %s\n" file line
    (FrontendTyped.layout_rty rty1)
    (FrontendTyped.layout_rty rty2)

let _warinning_subtyping_emptyness_error file line rty1 =
  Env.show_debug_typing @@ fun _ ->
  Pp.printf "@{<bold>Type Error at [%s::%i]:@} %s is empty type\n" file line
    (FrontendTyped.layout_rty rty1)

let _warinning_typing_error file line (str, rty) =
  Env.show_debug_typing @@ fun _ ->
  Pp.printf "@{<bold>Type Error at [%s::%i]:@} %s : %s\n" file line str
    (FrontendTyped.layout_rty rty)

let sub_rty_bool uctx (t1, t2) =
  let _ = pprint_typectx_subtyping uctx.local_ctx (t1, t2) in
  Subrty.sub_rty_bool uctx (t1, t2)

let is_nonempty_rty uctx t1 =
  let _ = pprint_typectx_nonempty uctx.local_ctx t1 in
  true
(* Subrty.is_nonempty_rty uctx t1 *)

let _id_type_infer file line (uctx : uctx) (id : string) : t rty =
  match get_opt uctx id with
  | None -> _failatwith file line (spf "cannot find %s in type context" id)
  | Some res -> res

let rec value_type_infer (uctx : uctx) (a : (t, t value) typed) :
    (t rty, t rty value) typed =
  let res =
    match a.x with
    | VVar id ->
        let res = _id_type_infer __FILE__ __LINE__ uctx id.x in
        let rty =
          match res with
          | RtyBaseArr _ | RtyArrArr _ -> res
          | RtyTuple _ -> _failatwith __FILE__ __LINE__ "unimp"
          | RtyBase _ -> mk_rty_var_eq_var a.ty (default_v, id.x)
        in
        (VVar id.x #: rty) #: rty
    | VConst U -> (VConst U) #: (prop_to_rty false Nt.unit_ty mk_true)
    | VConst c ->
        let rty = mk_rty_var_eq_c a.ty (default_v, c) in
        (VConst c) #: rty
    | VLam _ | VFix _ | VTu _ -> _failatwith __FILE__ __LINE__ "unimp"
  in
  let () = pprint_simple_typectx_infer uctx (layout_typed_value a, res.ty) in
  res

and value_type_check (uctx : uctx) (a : (t, t value) typed) (rty : t rty) :
    (t rty, t rty value) typed option =
  let () = pprint_simple_typectx_judge uctx (layout_typed_value a, rty) in
  match (a.x, rty) with
  | VConst _, _ | VVar _, _ ->
      let e = value_type_infer uctx a in
      if sub_rty_bool uctx (e.ty, rty) then Some e
      else (
        _warinning_subtyping_error __FILE__ __LINE__ (e.ty, rty);
        _warinning_typing_error __FILE__ __LINE__ (layout_typed_value a, rty);
        None)
  | VLam { lamarg; body }, RtyBaseArr { argcty; arg; retty } ->
      let body =
        body #-> (subst_term_instance lamarg.x (VVar arg #: lamarg.ty))
      in
      let argrty = RtyBase { ou = true; cty = argcty } in
      let* body =
        term_type_check (add_to_right uctx arg #: argrty) body retty
      in
      let lamarg = arg #: argrty in
      Some (VLam { lamarg; body }) #: rty
  | VLam { lamarg; body }, RtyArrArr { argrty; retty } ->
      let* body =
        term_type_check (add_to_right uctx lamarg.x #: argrty) body retty
      in
      let lamarg = lamarg.x #: argrty in
      Some (VLam { lamarg; body }) #: rty
  | VLam _, _ -> _failatwith __FILE__ __LINE__ ""
  | VFix { fixname; fixarg; body }, RtyBaseArr { argcty; arg; retty } ->
      let rec_constraint_cty = apply_rec_arg arg #: fixarg.ty in
      let () = init_cur_rec_func_name (fixname.x, rec_constraint_cty) in
      let rty' =
        let a = { x = Rename.unique arg; ty = fixarg.ty } in
        RtyBaseArr
          {
            argcty = intersect_ctys [ argcty; rec_constraint_cty ];
            arg = a.x;
            retty = subst_rty_instance arg (AVar a) retty;
          }
      in
      let binding = arg #: (RtyBase { ou = true; cty = argcty }) in
      let body =
        body #-> (subst_term_instance fixarg.x (VVar arg #: fixarg.ty))
      in
      let* body' =
        term_type_check
          (add_to_rights uctx [ binding; fixname.x #: rty' ])
          body retty
      in
      Some
        (VFix { fixname = fixname.x #: rty; fixarg = binding; body = body' })
        #: rty
  | VFix _, _ -> _failatwith __FILE__ __LINE__ ""
  | VTu _, _ -> _failatwith __FILE__ __LINE__ ""

and match_case_type_infer (uctx : uctx) (matched : (t, t value) typed)
    (x : t match_case) : t rty match_case option =
  match x with
  | CMatchcase { constructor; args; exp } ->
      let constructor_rty =
        _id_type_infer __FILE__ __LINE__ uctx
          (dt_name_for_typectx constructor.x)
      in
      let args, retty =
        List.fold_left
          (fun (args, rty) x ->
            match rty with
            | RtyBaseArr { argcty; arg; retty } ->
                let retty = subst_rty_instance arg (AVar x) retty in
                let x = x.x #: (RtyBase { ou = true; cty = argcty }) in
                (args @ [ x ], retty)
            | RtyArrArr { argrty; retty } ->
                let x = x.x #: argrty in
                (args @ [ x ], retty)
            | _ -> _failatwith __FILE__ __LINE__ "die")
          ([], constructor_rty) args
      in
      let retty =
        match retty with
        | RtyBase { ou = false; cty = Cty { phi; _ } } ->
            let lit =
              Checkaux.typed_value_to_typed_lit __FILE__ __LINE__ matched
            in
            let phi = subst_prop_instance default_v lit.x phi in
            RtyBase { ou = false; cty = Cty { nty = Nt.unit_ty; phi } }
        | _ -> _failatwith __FILE__ __LINE__ "die"
      in
      let dummy = (Rename.unique "dummy") #: retty in
      let bindings = args @ [ dummy ] in
      let* exp = term_type_infer (add_to_rights uctx bindings) exp in
      (* let _ = *)
      (*   Printf.printf "exists %s\n" *)
      (*   @@ List.split_by_comma (fun x -> x.x) bindings *)
      (* in *)
      let exp = exp.x #: (exists_rtys_to_rty bindings exp.ty) in
      Some
        (CMatchcase
           { constructor = constructor.x #: constructor_rty; args; exp })

and arrow_type_apply (uctx : uctx) appf_rty apparg =
  match appf_rty with
  | RtyBaseArr { argcty; arg; retty } ->
      (* NOTE: we need to capture the constraint from the argument type *)
      (* let argrty = and_cty_to_rty argcty apparg.ty in *)
      let argrty =
        mk_rty_var_eq_v (default_v, apparg.x #: (erase_rty apparg.ty))
      in
      let argrty = and_cty_to_rty argcty argrty in
      if is_nonempty_rty uctx argrty then
        let tmp_name = Rename.unique arg in

        (* let lit = *)
        (*   Checkaux. __FILE__ __LINE__ *)

        (* in *)
        let retty =
          subst_rty_instance arg (AVar tmp_name #: (erase_rty argrty)) retty
        in
        (* let cty = *)
        (*   match argcty with *)
        (*   | Cty { phi; _ } -> *)
        (*       Cty *)
        (*         { *)
        (*           nty = Nt.unit_ty; *)
        (*           phi = subst_prop_instance default_v lit.x phi; *)
        (*         } *)
        (* in *)
        (* let rty = RtyBase { ou = false; cty } in *)
        Some ([ tmp_name #: argrty ], retty)
      else (
        _warinning_subtyping_emptyness_error __FILE__ __LINE__ argrty;
        _warinning_typing_error __FILE__ __LINE__
          ( layout_typed_value apparg #-> (map_value erase_rty) #=> erase_rty,
            argrty );
        None)
  | RtyArrArr { argrty; retty } ->
      if sub_rty_bool uctx (apparg.ty, argrty) then Some ([], retty)
      else (
        _warinning_subtyping_error __FILE__ __LINE__ (apparg.ty, argrty);
        _warinning_typing_error __FILE__ __LINE__
          ( layout_typed_value apparg #-> (map_value erase_rty) #=> erase_rty,
            argrty );
        None)
  | _ -> _failatwith __FILE__ __LINE__ "type error: not an arrow type"

and term_type_infer_app (uctx : uctx) (a : ('t, 't term) typed) :
    ((t rty, string) typed list * (t rty, t rty term) typed) option =
  let res =
    match a.x with
    | CApp { appf; apparg } ->
        let appf, apparg = map2 (value_type_infer uctx) (appf, apparg) in
        (* HACK: safety check here *)
        let () =
          match get_cur_rec_func_name () with
          | Some (fname, rec_arg_rty) -> (
              match appf.x with
              | VVar x when String.equal fname x.x ->
                  if sub_rty_bool uctx (rec_arg_rty, apparg.ty) then ()
                  else (
                    _warinning_subtyping_error __FILE__ __LINE__
                      (rec_arg_rty, apparg.ty);
                    ( Env.show_debug_typing @@ fun _ ->
                      Pp.printf
                        "@{<bold>Recursive Safety Check Fails at [%s::%i]:@}\n"
                        __FILE__ __LINE__ );
                    raise RecArgCheckFailure)
              | _ -> ())
          | _ -> ()
        in
        let* bindings, retty = arrow_type_apply uctx appf.ty apparg in
        Some (bindings, (CApp { appf; apparg }) #: retty)
    | CAppOp { op; appopargs } ->
        let op_rty =
          _id_type_infer __FILE__ __LINE__ uctx (op_name_for_typectx op.x)
        in
        let op = op.x #: op_rty in
        let appopargs = List.map (value_type_infer uctx) appopargs in
        let* bindings, res =
          List.fold_left
            (fun res apparg ->
              let* bindings, op_rty = res in
              let* bindings', op_rty = arrow_type_apply uctx op_rty apparg in
              Some (bindings @ bindings', op_rty))
            (Some ([], op_rty))
            appopargs
        in
        Some (bindings, (CAppOp { op; appopargs }) #: res)
    | _ ->
        let* res = term_type_infer uctx a in
        Some ([], res)
  in
  res

and term_type_infer (uctx : uctx) (a : ('t, 't term) typed) :
    (t rty, t rty term) typed option =
  let res =
    match a.x with
    | CErr -> Some CErr #: (prop_to_rty false a.ty mk_false)
    | CVal v ->
        let v = value_type_infer uctx v in
        Some (CVal v) #: v.ty
    | CApp _ | CAppOp _ ->
        let* bindings, res = term_type_infer_app uctx a in
        Some res.x #: (exists_rtys_to_rty bindings res.ty)
    | CMatch { matched; match_cases } ->
        (* NOTE: we drop unreachable cases *)
        let match_cases =
          List.filter_map (match_case_type_infer uctx matched) match_cases
        in
        (* let* match_cases = Sugar.opt_list_to_list_opt match_cases in *)
        let unioned_ty =
          union_rtys
          @@ List.map (function CMatchcase { exp; _ } -> exp.ty) match_cases
        in
        let matched = value_type_infer uctx matched in
        Some (CMatch { matched; match_cases }) #: unioned_ty
    | CLetDeTu _ -> failwith "unimp"
    | CLetE { rhs; lhs; body } ->
        let* bindings, rhs = term_type_infer_app uctx rhs in
        let lhs = lhs.x #: rhs.ty in
        let bindings = bindings @ [ lhs ] in
        let* body = term_type_infer (add_to_rights uctx bindings) body in
        (* let _ = *)
        (*   Printf.printf "CLetE exists %s\n" *)
        (*   @@ List.split_by_comma (fun x -> x.x) bindings *)
        (* in *)
        Some (CLetE { rhs; lhs; body }) #: (exists_rtys_to_rty bindings body.ty)
  in
  let () =
    match res with
    | Some res -> pprint_simple_typectx_infer uctx (layout_typed_term a, res.ty)
    | None -> ()
  in
  res

and term_type_check (uctx : uctx) (y : ('t, 't term) typed) (rty : t rty) :
    (t rty, t rty term) typed option =
  let () = pprint_simple_typectx_judge uctx (layout_typed_term y, rty) in
  match y.x with
  | CErr -> Some CErr #: rty
  | CLetDeTu _ -> failwith "unimp"
  | CVal v ->
      let* v = value_type_check uctx v rty in
      Some (CVal v) #: rty
  | CApp _ | CAppOp _ | CMatch _ ->
      let* x = term_type_infer uctx y in
      (* let () = failwith "end" in *)
      if sub_rty_bool uctx (x.ty, rty) then Some x.x #: rty
      else (
        _warinning_subtyping_error __FILE__ __LINE__ (x.ty, rty);
        _warinning_typing_error __FILE__ __LINE__ (layout_typed_term y, rty);
        None)
  | CLetE { rhs; lhs; body } ->
      let* bindings, rhs = term_type_infer_app uctx rhs in
      let lhs = lhs.x #: rhs.ty in
      let bindings = bindings @ [ lhs ] in
      let* body = term_type_check (add_to_rights uctx bindings) body rty in
      (* let _ = *)
      (*   Printf.printf "CLetE exists %s\n" *)
      (*   @@ List.split_by_comma (fun x -> x.x) bindings *)
      (* in *)
      Some (CLetE { rhs; lhs; body }) #: rty

let term_type_check_with_rec_check (uctx : uctx) (y : ('t, 't term) typed)
    (rty : t rty) =
  try term_type_check uctx y rty with RecArgCheckFailure -> None

let value_type_check_with_rec_check (uctx : uctx) (a : (t, t value) typed)
    (rty : t rty) =
  try value_type_check uctx a rty with RecArgCheckFailure -> None
