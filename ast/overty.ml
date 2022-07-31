module T = struct
  open Sexplib.Std
  open Sugar

  type id = Strid.T.t [@@deriving sexp]
  type normalty = Normalty.T.t [@@deriving sexp]

  type t =
    | OverTy_base of { basename : id; normalty : normalty; prop : Autov.Prop.t }
    | OverTy_arrow of { argname : id; argty : t; retty : t }
    | OverTy_tuple of t list
  [@@deriving sexp]

  let rec destruct_arrow_tp = function
    | OverTy_arrow { argname; argty; retty; _ } ->
        let a, b = destruct_arrow_tp retty in
        ((argty, argname) :: a, b)
    | ty -> ([], ty)

  let rec erase = function
    | OverTy_base { normalty; _ } -> normalty
    | OverTy_arrow { argty; retty; _ } ->
        Normalty.T.Ty_arrow (erase argty, erase retty)
    | OverTy_tuple ts -> Normalty.T.Ty_tuple (List.map erase ts)

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

  let mk_int_id name = P.{ ty = T.Int; x = name }

  let make_basic basename normalty prop =
    OverTy_base
      {
        basename;
        normalty;
        prop = prop P.{ ty = Normalty.T.to_smtty normalty; x = basename };
      }

  let make_basic_top basename normalty =
    make_basic basename normalty (fun _ -> P.mk_true)

  let make_arrow argname normalty argtyf rettyf =
    let id = P.{ ty = Normalty.T.to_smtty normalty; x = argname } in
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
        let smtty = Normalty.T.to_smtty normalty in
        let x = P.{ ty = smtty; x = xname } in
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
end
