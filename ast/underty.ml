module T = struct
  open Sexplib.Std
  open Sugar

  type id = Strid.T.t [@@deriving sexp]
  type normalty = Normalty.T.t [@@deriving sexp]

  type t =
    | UnderTy_base of {
        basename : id;
        normalty : normalty;
        prop : Autov.Prop.t;
      }
    | UnderTy_arrow of { argname : id; argty : t; retty : t }
    | UnderTy_tuple of t list
  [@@deriving sexp]

  (* let eq = failwith "unimp over eq" *)

  let rec destruct_arrow_tp = function
    | UnderTy_arrow { argname; argty; retty } ->
        let a, b = destruct_arrow_tp retty in
        ((argty, argname) :: a, b)
    | ty -> ([], ty)

  (* let construct_arrow_tp = failwith "unimp" *)

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

  let base_type_add_conjunction f = function
    | UnderTy_base { basename; normalty; prop } ->
        UnderTy_base
          { basename; normalty; prop = Autov.Prop.(And [ prop; f basename ]) }
    | _ -> failwith "base_type_add_conjunction"

  module P = Autov.Prop
  module T = Autov.Smtty

  let nu = "_nu"
  let mk_int_id name = P.{ ty = T.Int; x = name }

  let make_basic_top normalty =
    UnderTy_base { basename = nu; normalty; prop = P.True }

  let make_basic normalty propf =
    let basename = nu in
    let nu = P.{ ty = Normalty.T.to_smtty normalty; x = basename } in
    UnderTy_base { basename; normalty; prop = propf nu }

  let make_arrow argname argty rettyf =
    UnderTy_arrow
      {
        argname;
        argty;
        retty =
          rettyf P.{ ty = Normalty.T.to_smtty @@ erase argty; x = argname };
      }

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
      | _ -> failwith "arrow_args_rename"
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
    | _, _ -> failwith "disjunct_basetype"

  let strict_eq _ _ = true

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
      | _, _ -> at_failwith __FILE__ __LINE__
    in
    aux (t1, t2)

  let disjunct_list ts =
    match ts with
    | [] -> failwith "disjunct no types"
    | [ t ] -> t
    | h :: t -> List.fold_left disjunct h t
end
