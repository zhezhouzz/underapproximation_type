module T = struct
  open Sexplib.Std
  open Sugar
  module NT = Normalty.Ast.T

  type id = string [@@deriving sexp]
  type normalty = NT.t [@@deriving sexp]

  type t =
    | OverTy_base of { basename : id; normalty : normalty; prop : Autov.Prop.t }
    | OverTy_arrow of { argname : id; argty : t; retty : t }
    | OverTy_tuple of t list
  [@@deriving sexp]

  open Zzdatatype.Datatype

  let var_space t =
    let add xs s = List.fold_left (fun s x -> StrMap.add x () s) s xs in
    let rec aux s = function
      | OverTy_base { basename; prop; _ } ->
          StrMap.add basename () @@ add (Autov.Prop.var_space prop) s
      | OverTy_tuple ts -> List.fold_left aux s ts
      | OverTy_arrow { argname; argty; retty } ->
          aux (aux (StrMap.add argname () s) argty) retty
    in
    StrMap.to_key_list @@ aux StrMap.empty t

  let fv bodyt =
    let rec aux = function
      | OverTy_base { basename; prop; _ } ->
          let fv = Autov.prop_fv prop in
          List.filter (fun x -> not @@ String.equal basename x) fv
      | OverTy_tuple ts -> List.concat (List.map aux ts)
      | OverTy_arrow { argname; argty; retty } ->
          let fv = aux retty in
          let fv = List.filter (fun x -> not @@ String.equal argname x) fv in
          Zzdatatype.Datatype.List.slow_rm_dup String.equal (fv @ aux argty)
    in
    aux bodyt

  let rec destruct_arrow_tp = function
    | OverTy_arrow { argname; argty; retty; _ } ->
        let a, b = destruct_arrow_tp retty in
        ((argty, argname) :: a, b)
    | ty -> ([], ty)

  let rec erase = function
    | OverTy_base { normalty; _ } -> normalty
    | OverTy_arrow { argty; retty; _ } -> NT.Ty_arrow (erase argty, erase retty)
    | OverTy_tuple ts -> NT.Ty_tuple (List.map erase ts)

  let subst_id t x y =
    let rec aux t =
      match t with
      | OverTy_base { basename; normalty; prop } ->
          if String.equal basename x then t
          else
            OverTy_base
              { basename; normalty; prop = Autov.Prop.subst_id prop x y }
      | OverTy_arrow { argname; argty; retty } ->
          let argty = aux argty in
          let retty = if String.equal argname x then retty else aux retty in
          OverTy_arrow { argname; argty; retty }
      | OverTy_tuple ts -> OverTy_tuple (List.map aux ts)
    in
    aux t

  let base_type_add_conjunction f = function
    | OverTy_base { basename; normalty; prop } ->
        OverTy_base
          { basename; normalty; prop = Autov.Prop.(And [ prop; f basename ]) }
    | _ -> _failatwith __FILE__ __LINE__ ""

  module P = Autov.Prop
  module T = Autov.Smtty
  module Ntyped = Normalty.Ast.Ntyped
  open Ntyped

  let strict_eq t1 t2 =
    let rec aux (t1, t2) =
      match (t1, t2) with
      | ( OverTy_base
            { basename = basename1; normalty = normalty1; prop = prop1 },
          OverTy_base
            { basename = basename2; normalty = normalty2; prop = prop2 } ) ->
          String.equal basename1 basename2
          && NT.eq normalty1 normalty2 && P.strict_eq prop1 prop2
      | OverTy_tuple ts1, OverTy_tuple ts2 ->
          List.for_all aux @@ _safe_combine __FILE__ __LINE__ ts1 ts2
      | ( OverTy_arrow { argname = argname1; argty = argty1; retty = retty1 },
          OverTy_arrow { argname = argname2; argty = argty2; retty = retty2 } )
        ->
          String.equal argname1 argname2
          && aux (argty1, argty2)
          && aux (retty1, retty2)
      | _, _ -> false
    in
    aux (t1, t2)

  let eq a b = strict_eq a b
  let mk_int_id name = { ty = NT.Ty_int; x = name }

  let make_basic basename normalty prop =
    OverTy_base
      { basename; normalty; prop = prop { ty = normalty; x = basename } }

  let make_basic_top basename normalty =
    make_basic basename normalty (fun _ -> P.mk_true)

  let make_arrow argname normalty argtyf rettyf =
    let id = { ty = normalty; x = argname } in
    OverTy_arrow { argname; argty = argtyf argname normalty; retty = rettyf id }

  let arrow_args_rename args overftp =
    let rec aux args overftp =
      match (args, overftp) with
      | [], tp -> tp
      | id :: args, OverTy_arrow { argname; argty; retty } ->
          OverTy_arrow
            {
              argname = id;
              argty;
              retty = aux args @@ subst_id retty argname id;
            }
      | _ -> _failatwith __FILE__ __LINE__ ""
    in
    aux args overftp

  let is_base_type = function OverTy_base _ -> true | _ -> false

  let forall_quantify_variable_in_ty xname xty ty =
    match xty with
    | OverTy_arrow _ -> _failatwith __FILE__ __LINE__ "arrow type"
    | OverTy_tuple _ -> _failatwith __FILE__ __LINE__ "tuple type"
    | OverTy_base { basename; normalty; prop } ->
        let xprop = P.subst_id prop basename xname in
        let x = { x = xname; ty = normalty } in
        let rec aux ty =
          match ty with
          | OverTy_base { basename; normalty; prop } ->
              let fv = Autov.prop_fv prop in
              if List.exists (fun x' -> String.equal x.x x') fv then
                OverTy_base
                  {
                    basename;
                    normalty;
                    prop = P.Forall (x, Implies (xprop, prop));
                  }
              else ty
          | OverTy_tuple ts -> OverTy_tuple (List.map aux ts)
          (* TODO: what happen when it is a arrow type *)
          | OverTy_arrow _ -> _failatwith __FILE__ __LINE__ "unimp"
        in
        aux ty

  let instantiate_vars (x, lit) t =
    let rec aux t =
      match t with
      | OverTy_base { basename; normalty; prop } ->
          if String.equal basename x then t
          else
            OverTy_base
              {
                basename;
                normalty;
                prop = Autov.Prop.instantiate_vars (x, lit) prop;
              }
      | OverTy_arrow { argname; argty; retty } ->
          let argty = aux argty in
          let retty = if String.equal argname x then retty else aux retty in
          OverTy_arrow { argname; argty; retty }
      | OverTy_tuple ts -> OverTy_tuple (List.map aux ts)
    in
    aux t

  let modify_prop_over_basetype f = function
    | ( OverTy_base { basename = basename1; normalty = normalty1; prop = prop1 },
        OverTy_base { basename = basename2; normalty = normalty2; prop = prop2 }
      ) ->
        let normalty =
          _check_equality __FILE__ __LINE__ NT.eq normalty1 normalty2
        in
        let prop2 =
          if String.equal basename1 basename2 then prop2
          else P.subst_id prop2 basename2 basename1
        in
        OverTy_base { basename = basename1; normalty; prop = f prop1 prop2 }
    | _, _ -> _failatwith __FILE__ __LINE__ ""

  let modify_prop_over_ty f t1 t2 =
    let rec aux (t1, t2) =
      match (t1, t2) with
      | OverTy_base _, OverTy_base _ -> modify_prop_over_basetype f (t1, t2)
      | OverTy_tuple ts1, OverTy_tuple ts2 ->
          OverTy_tuple (List.map aux @@ _safe_combine __FILE__ __LINE__ ts1 ts2)
      | ( OverTy_arrow { argname = argname1; argty = argty1; retty = retty1 },
          OverTy_arrow { argname = argname2; argty = argty2; retty = retty2 } )
        ->
          (* NOTE: we ask the argument should be exactly the same *)
          let argname =
            _check_equality __FILE__ __LINE__ String.equal argname1 argname2
          in
          let argty =
            _check_equality __FILE__ __LINE__ strict_eq argty1 argty2
          in
          let retty = aux (retty1, retty2) in
          OverTy_arrow { argname; argty; retty }
      | _, _ -> _failatwith __FILE__ __LINE__ ""
    in
    aux (t1, t2)

  let disjunct = modify_prop_over_ty (fun x y -> P.Or [ x; y ])
  let conjunct = modify_prop_over_ty (fun x y -> P.And [ x; y ])

  let disjunct_list ts =
    match ts with
    | [] -> _failatwith __FILE__ __LINE__ "disjunct no types"
    | [ t ] -> t
    | h :: t -> List.fold_left disjunct h t

  let conjunct_list ts =
    match ts with
    | [] -> _failatwith __FILE__ __LINE__ "conjunct no types"
    | [ t ] -> t
    | h :: t -> List.fold_left conjunct h t
end

module Otyped = struct
  include T

  type 'a typed = { x : 'a; ty : t } [@@deriving sexp]

  (* let map (f : 'a -> 'b) { x; ty } = { x = f x; ty } *)
  (* let typed_eq a b = String.equal a.x b.x && eq a.ty b.ty *)
end
