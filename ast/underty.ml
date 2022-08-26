module T = struct
  open Sexplib.Std
  open Sugar

  type id = Strid.T.t [@@deriving sexp]
  type normalty = Normalty.T.t [@@deriving sexp]

  open Typed.F (Normalty.T)

  type t =
    | UnderTy_base of {
        basename : id;
        normalty : normalty;
        prop : Autov.Prop.t;
      }
    | UnderTy_arrow of { argname : id; argty : t; retty : t }
    | UnderTy_tuple of t list
  [@@deriving sexp]

  open Zzdatatype.Datatype

  let var_space t =
    let add xs s = List.fold_left (fun s x -> StrMap.add x () s) s xs in
    let rec aux s = function
      | UnderTy_base { basename; prop; _ } ->
          let space =
            List.filter (String.equal basename) (Autov.Prop.var_space prop)
          in
          add space s
      | UnderTy_tuple ts -> List.fold_left aux s ts
      | UnderTy_arrow { argname; argty; retty } ->
          let space =
            StrMap.to_key_list @@ aux (aux StrMap.empty argty) retty
          in
          let space = List.filter (String.equal argname) space in
          add space s
    in
    StrMap.to_key_list @@ aux StrMap.empty t

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

  let assume_base_destruct_opt = function
    | UnderTy_base { basename; normalty; prop } ->
        Some (basename, normalty, prop)
    | _ -> None

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

  let eq a b = strict_eq a b

  let modify_prop_over_basetype f = function
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
        UnderTy_base { basename = basename1; normalty; prop = f prop1 prop2 }
    | _, _ -> _failatwith __FILE__ __LINE__ ""

  let modify_prop_in_ty f t1 t2 =
    let rec aux (t1, t2) =
      match (t1, t2) with
      | UnderTy_base _, UnderTy_base _ -> modify_prop_over_basetype f (t1, t2)
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

  let disjunct = modify_prop_in_ty (fun x y -> P.Or [ x; y ])
  let conjunct = modify_prop_in_ty (fun x y -> P.And [ x; y ])

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

  let fv bodyt =
    let rec aux = function
      | UnderTy_base { basename; prop; _ } ->
          let fv = Autov.prop_fv prop in
          List.filter (fun x -> not @@ String.equal basename x) fv
      | UnderTy_tuple ts -> List.concat (List.map aux ts)
      | UnderTy_arrow { argname; argty; retty } ->
          let fv = aux retty in
          let fv = List.filter (fun x -> not @@ String.equal argname x) fv in
          Zzdatatype.Datatype.List.slow_rm_dup String.equal (fv @ aux argty)
    in
    aux bodyt

  let map_on_retty f t =
    let rec aux t =
      match t with
      | UnderTy_base { basename; normalty; prop } ->
          UnderTy_base { basename; normalty; prop = f prop }
      | UnderTy_tuple ts -> UnderTy_tuple (List.map aux ts)
      | UnderTy_arrow { argname; argty; retty } ->
          UnderTy_arrow { argname; argty; retty = aux retty }
    in
    aux t

  let eqv_to_bodyt { ty; x } =
    UnderTy_base { basename = x; normalty = ty; prop = Autov.Prop.mk_true }

  let join_tuple_t t = function
    | UnderTy_tuple ts -> UnderTy_tuple (ts @ [ t ])
    | _ -> _failatwith __FILE__ __LINE__ ""
end
