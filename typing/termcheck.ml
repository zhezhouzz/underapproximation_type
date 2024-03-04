open Languagez
open FrontendTyped
open Zzdatatype.Datatype
open Sugar
open Subtyping

type t = Nt.t

let layout_ty = Nt.layout

let _id_type_infer file line (uctx : t rty ctx) (id : string) : t rty =
  match get_opt uctx id with
  | None -> _failatwith file line "cannot find var in type context"
  | Some res -> res

let rec value_type_infer (uctx : t rty ctx) (a : (t, t value) typed) :
    (t rty, t rty value) typed =
  match a.x with
  | VVar id ->
      let res = _id_type_infer __FILE__ __LINE__ uctx id.x in
      let rty =
        match res with
        | RtyBaseArr _ | RtyArrArr _ | RtyBase { ou = false; _ } -> res
        | RtyTuple _ -> _failatwith __FILE__ __LINE__ "unimp"
        | RtyBase { ou = true; _ } -> mk_rty_var_eq_var a.ty (default_v, id.x)
      in
      (VVar id.x #: rty) #: rty
  | VConst c ->
      let rty = mk_rty_var_eq_c a.ty (default_v, c) in
      (VConst c) #: rty
  | VLam _ | VFix _ | VTu _ -> _failatwith __FILE__ __LINE__ "unimp"

and value_type_check (uctx : t rty ctx) (a : (t, t value) typed) (rty : t rty) :
    (t rty, t rty value) typed =
  match (a.x, rty) with
  | VConst _, _ | VVar _, _ ->
      let e = value_type_infer uctx a in
      if Subrty.sub_rty_bool uctx (e.ty, rty) then e
      else _failatwith __FILE__ __LINE__ "value_type_check"
  | VLam { lamarg; body }, RtyBaseArr { argcty; arg; retty } ->
      let retty = subst_rty_instance arg (AVar lamarg) retty in
      let argrty = RtyBase { ou = true; cty = argcty } in
      let body =
        term_type_check (add_to_right uctx lamarg.x #: argrty) body retty
      in
      let lamarg = lamarg.x #: argrty in
      (VLam { lamarg; body }) #: rty
  | VLam { lamarg; body }, RtyArrArr { argrty; retty } ->
      let body =
        term_type_check (add_to_right uctx lamarg.x #: argrty) body retty
      in
      let lamarg = lamarg.x #: argrty in
      (VLam { lamarg; body }) #: rty
  | VLam _, _ -> _failatwith __FILE__ __LINE__ ""
  | VFix { fixname; fixarg; body }, RtyBaseArr { argcty; arg; retty } ->
      let a = { x = Rename.unique fixarg.x; ty = fixarg.ty } in
      let prop = Checkaux.make_order_constraint a fixarg in
      let rty = map_prop_in_retrty (fun prop' -> smart_add_to prop prop') rty in
      let binding_a = a.x #: (RtyBase { ou = true; cty = argcty }) in
      let retty = subst_rty_instance arg (AVar a) retty in
      let body = (subst_term_instance fixarg.x (VVar a) body.x) #: body.ty in
      let body' =
        term_type_check
          (add_to_rights uctx [ binding_a; fixname.x #: rty ])
          body retty
      in
      (VFix
         {
           fixname = fixname.x #: rty;
           fixarg = fixname.x #: binding_a.ty;
           body = body';
         })
      #: rty
  | VFix _, _ -> _failatwith __FILE__ __LINE__ ""
  | VTu _, _ -> _failatwith __FILE__ __LINE__ ""

and arrow_type_apply uctx appf_rty apparg =
  let lit =
    Checkaux.typed_value_to_typed_lit __FILE__ __LINE__
      apparg.x #: (erase_rty apparg.ty)
  in
  match appf_rty with
  | RtyBaseArr { argcty; arg; retty } ->
      if
        Subrty.sub_rty_bool uctx (apparg.ty, RtyBase { ou = true; cty = argcty })
      then retty
      else
        let retty = subst_rty_instance arg lit.x retty in
        retty
  | RtyArrArr { argrty; retty } ->
      if Subrty.sub_rty_bool uctx (apparg.ty, argrty) then retty
      else _failatwith __FILE__ __LINE__ "type error"
  | _ -> _failatwith __FILE__ __LINE__ "type error: not an arrow type"

and term_type_infer (uctx : t rty ctx) (a : ('t, 't term) typed) :
    (t rty, t rty term) typed =
  let res =
    match a.x with
    | CErr -> CErr #: (prop_to_rty false a.ty mk_true)
    | CVal v ->
        let v = value_type_infer uctx v in
        (CVal v) #: v.ty
    | CApp { appf; apparg } ->
        let appf, apparg = map2 (value_type_infer uctx) (appf, apparg) in
        let retty = arrow_type_apply uctx appf.ty apparg in
        (CApp { appf; apparg }) #: retty
    | CAppOp { op; appopargs } ->
        let op_rty =
          _id_type_infer __FILE__ __LINE__ uctx (op_name_for_typectx op.x)
        in
        let op = op.x #: op_rty in
        let appopargs = List.map (value_type_infer uctx) appopargs in
        let res =
          List.fold_left
            (fun op_rty apparg -> arrow_type_apply uctx op_rty apparg)
            op_rty appopargs
        in
        (CAppOp { op; appopargs }) #: res
    | CMatch _ | CLetDeTu _ -> failwith "unimp"
    | CLetE { rhs; lhs; body } ->
        let rhs = term_type_infer uctx rhs in
        let lhs = lhs.x #: rhs.ty in
        let body = term_type_infer (add_to_right uctx lhs) body in
        (CLetE { rhs; lhs; body }) #: body.ty
  in
  (* let () = pretty_print_infer uctx.ctx (layout a, res) in *)
  res

and term_type_check (uctx : t rty ctx) (x : ('t, 't term) typed) (rty : t rty) :
    (t rty, t rty term) typed =
  (* let () = pretty_print_judge uctx.ctx (layout x, ty) in *)
  match x.x with
  | CErr -> CErr #: rty
  | CVal v ->
      let v = value_type_check uctx v rty in
      (CVal v) #: v.ty
  | CLetDeTu _ | CMatch _ -> failwith "unimp"
  | CApp _ | CAppOp _ ->
      let x = term_type_infer uctx x in
      if Subrty.sub_rty_bool uctx (x.ty, rty) then x.x #: rty
      else _failatwith __FILE__ __LINE__ "type error"
  | CLetE { rhs; lhs; body } ->
      let rhs = term_type_infer uctx rhs in
      let lhs = lhs.x #: rhs.ty in
      let body = term_type_check (add_to_right uctx lhs) body rty in
      (CLetE { rhs; lhs; body }) #: rty
