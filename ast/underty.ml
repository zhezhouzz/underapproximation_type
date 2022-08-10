module T = struct
  open Sexplib.Std
  open Sugar

  type id = Strid.T.t [@@deriving sexp]
  type normalty = Normalty.T.t [@@deriving sexp]
  type 'a typed = { ty : normalty; x : 'a } [@@deriving sexp]

  type t =
    | UnderTy_base of {
        basename : id;
        normalty : normalty;
        prop : Autov.Prop.t;
      }
    | UnderTy_arrow of { argname : id; argty : t; retty : t }
    | UnderTy_tuple of t list
  [@@deriving sexp]

  type qt = id typed list * id typed list * t

  let rec destruct_arrow_tp = function
    | UnderTy_arrow { argname; argty; retty } ->
        let a, b = destruct_arrow_tp retty in
        ((argty, argname) :: a, b)
    | ty -> ([], ty)

  let rec erase = function
    | UnderTy_base { normalty; _ } -> normalty
    | UnderTy_arrow { argty; retty; _ } ->
        Normalty.T.Ty_arrow (erase argty, erase retty)
    | UnderTy_tuple ts -> Normalty.T.Ty_tuple (List.map erase ts)

  let subst_id t x y =
    let rec aux t =
      match t with
      | UnderTy_base { basename; normalty; prop } ->
          if String.equal basename x then t
          else
            UnderTy_base
              { basename; normalty; prop = Autov.Prop.subst_id prop x y }
      | UnderTy_arrow { argname; argty; retty } ->
          let argty = aux argty in
          let retty = if String.equal argname x then retty else aux retty in
          UnderTy_arrow { argname; argty; retty }
      | UnderTy_tuple ts -> UnderTy_tuple (List.map aux ts)
    in
    aux t

  let base_type_add_conjunction_with_selfname f = function
    | UnderTy_base { basename; normalty; prop } ->
        UnderTy_base
          { basename; normalty; prop = Autov.Prop.(And [ prop; f basename ]) }
    | _ -> _failatwith __FILE__ __LINE__ ""

  let base_type_add_implication c = function
    | UnderTy_base { basename; normalty; prop } ->
        UnderTy_base
          { basename; normalty; prop = Autov.Prop.(Implies (c, prop)) }
    | _ -> _failatwith __FILE__ __LINE__ ""

  let base_type_add_conjunction c = function
    | UnderTy_base { basename; normalty; prop } ->
        UnderTy_base { basename; normalty; prop = Autov.Prop.(And [ c; prop ]) }
    | _ -> _failatwith __FILE__ __LINE__ ""

  let base_type_extract_prop = function
    | UnderTy_base { basename; prop; _ } -> (basename, prop)
    | _ -> _failatwith __FILE__ __LINE__ ""

  module P = Autov.Prop
  module T = Autov.Smtty

  let mk_int_id name = P.{ ty = T.Int; x = name }

  let make_basic basename normalty prop =
    UnderTy_base
      {
        basename;
        normalty;
        prop = prop P.{ ty = Normalty.T.to_smtty normalty; x = basename };
      }

  let make_basic_top basename normalty =
    make_basic basename normalty (fun _ -> P.mk_true)

  let make_arrow argname normalty argtyf rettyf =
    let id = P.{ ty = Normalty.T.to_smtty normalty; x = argname } in
    UnderTy_arrow
      { argname; argty = argtyf argname normalty; retty = rettyf id }

  let arrow_args_rename args overftp =
    let rec aux args overftp =
      match (args, overftp) with
      | [], tp -> tp
      | id :: args, UnderTy_arrow { argname; argty; retty } ->
          UnderTy_arrow
            {
              argname = id;
              argty;
              retty = aux args @@ subst_id retty argname id;
            }
      | _ -> _failatwith __FILE__ __LINE__ ""
    in
    aux args overftp

  let is_base_type = function UnderTy_base _ -> true | _ -> false

  let disjunct_basetype = function
    | ( UnderTy_base { basename = basename1; normalty = normalty1; prop = prop1 },
        UnderTy_base
          { basename = basename2; normalty = normalty2; prop = prop2 } ) ->
        let normalty =
          _check_equality __FILE__ __LINE__ Normalty.T.eq normalty1 normalty2
        in
        let prop2 =
          if String.equal basename1 basename2 then prop2
          else P.subst_id prop2 basename2 basename1
        in
        UnderTy_base
          { basename = basename1; normalty; prop = P.Or [ prop1; prop2 ] }
    | _, _ -> _failatwith __FILE__ __LINE__ ""

  let strict_eq t1 t2 =
    let rec aux (t1, t2) =
      match (t1, t2) with
      | ( UnderTy_base
            { basename = basename1; normalty = normalty1; prop = prop1 },
          UnderTy_base
            { basename = basename2; normalty = normalty2; prop = prop2 } ) ->
          String.equal basename1 basename2
          && Normalty.T.eq normalty1 normalty2
          && P.strict_eq prop1 prop2
      | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
          List.for_all aux @@ _safe_combine __FILE__ __LINE__ ts1 ts2
      | ( UnderTy_arrow { argname = argname1; argty = argty1; retty = retty1 },
          UnderTy_arrow { argname = argname2; argty = argty2; retty = retty2 } )
        ->
          String.equal argname1 argname2
          && aux (argty1, argty2)
          && aux (retty1, retty2)
      | _, _ -> false
    in
    aux (t1, t2)

  let disjunct t1 t2 =
    let rec aux (t1, t2) =
      match (t1, t2) with
      | UnderTy_base _, UnderTy_base _ -> disjunct_basetype (t1, t2)
      | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
          UnderTy_tuple (List.map aux @@ _safe_combine __FILE__ __LINE__ ts1 ts2)
      | ( UnderTy_arrow { argname = argname1; argty = argty1; retty = retty1 },
          UnderTy_arrow { argname = argname2; argty = argty2; retty = retty2 } )
        ->
          (* NOTE: we ask the argument should be exactly the same *)
          let argname =
            _check_equality __FILE__ __LINE__ String.equal argname1 argname2
          in
          let argty =
            _check_equality __FILE__ __LINE__ strict_eq argty1 argty2
          in
          let retty = aux (retty1, retty2) in
          UnderTy_arrow { argname; argty; retty }
      | _, _ -> _failatwith __FILE__ __LINE__ ""
    in
    aux (t1, t2)

  let disjunct_list ts =
    match ts with
    | [] -> _failatwith __FILE__ __LINE__ "disjunct no types"
    | [ t ] -> t
    | h :: t -> List.fold_left disjunct h t

  let hide_exists_quantify_variable_in_prop x xprop (basename, normalty, prop) =
    UnderTy_base
      { basename; normalty; prop = P.Exists (x, And [ xprop; prop ]) }

  let hide_forall_quantify_variable_in_prop x xprop (basename, normalty, prop) =
    UnderTy_base
      { basename; normalty; prop = P.Forall (x, Implies (xprop, prop)) }

  let hide_quantify_variable_in_ty xname xty ty =
    match xty with
    | UnderTy_arrow _ -> _failatwith __FILE__ __LINE__ "arrow type"
    | UnderTy_tuple _ -> _failatwith __FILE__ __LINE__ "tuple type"
    | UnderTy_base { basename; normalty; prop } ->
        let xprop = P.subst_id prop basename xname in
        let smtty = Normalty.T.to_smtty normalty in
        let x = P.{ ty = smtty; x = xname } in
        (* let _ = *)
        (*   Printf.printf "make qv: %s --> %s:%s\n" xname xname *)
        (*     (Autov.Smtty.layout smtty) *)
        (* in *)
        let rec aux ty =
          match ty with
          | UnderTy_base { basename; normalty; prop } ->
              let fv = Autov.prop_fv prop in
              if List.exists (fun x' -> String.equal x.x x') fv then
                if Normalty.T.is_basic_tp normalty then
                  hide_exists_quantify_variable_in_prop x xprop
                    (basename, normalty, prop)
                else
                  hide_forall_quantify_variable_in_prop x xprop
                    (basename, normalty, prop)
              else ty
          | UnderTy_tuple ts -> UnderTy_tuple (List.map aux ts)
          (* TODO: what happen when it is a arrow type *)
          | UnderTy_arrow _ -> _failatwith __FILE__ __LINE__ "unimp"
        in
        aux ty

  let instantiate_vars (x, lit) t =
    let rec aux t =
      match t with
      | UnderTy_base { basename; normalty; prop } ->
          if String.equal basename x then t
          else
            UnderTy_base
              {
                basename;
                normalty;
                prop = Autov.Prop.instantiate_vars (x, lit) prop;
              }
      | UnderTy_arrow { argname; argty; retty } ->
          let argty = aux argty in
          let retty = if String.equal argname x then retty else aux retty in
          UnderTy_arrow { argname; argty; retty }
      | UnderTy_tuple ts -> UnderTy_tuple (List.map aux ts)
    in
    aux t
end
