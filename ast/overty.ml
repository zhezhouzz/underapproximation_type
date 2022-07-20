module T = struct
  open Sexplib.Std

  type id = Strid.T.t [@@deriving sexp]
  type normalty = Normalty.T.t [@@deriving sexp]

  type t =
    | OverTy_base of { basename : id; normalty : normalty; prop : Autov.Prop.t }
    | OverTy_arrow of { argname : id; argty : t; retty : t }
    | OverTy_tuple of t list
  [@@deriving sexp]

  let eq = failwith "unimp"
  let destruct_arrow_tp = failwith "unimp"
  let construct_arrow_tp = failwith "unimp"

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
end
