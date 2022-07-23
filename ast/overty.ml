module T = struct
  open Sexplib.Std

  type id = Strid.T.t [@@deriving sexp]
  type normalty = Normalty.T.t [@@deriving sexp]

  type t =
    | OverTy_base of { basename : id; normalty : normalty; prop : Autov.Prop.t }
    | OverTy_arrow of { argname : id; argty : t; retty : t }
    | OverTy_tuple of t list
  [@@deriving sexp]

  (* let eq = failwith "unimp over eq" *)

  let rec destruct_arrow_tp = function
    | OverTy_arrow { argname; argty; retty; _ } ->
        let a, b = destruct_arrow_tp retty in
        ((argty, argname) :: a, b)
    | ty -> ([], ty)

  (* let construct_arrow_tp = failwith "unimp" *)

  let basic_normalty_to_smtty t =
    let open Normalty.T in
    let aux = function
      | Ty_bool -> Autov.Smtty.Bool
      | Ty_list _ | Ty_tree _ | Ty_int -> Autov.Smtty.Int
      | _ -> failwith "to_smtty: not a basic type"
    in
    aux t

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
    | _ -> failwith "base_type_add_conjunction"

  module P = Autov.Prop
  module T = Autov.Smtty

  let nu = "_nu"
  let mk_int_id name = P.{ ty = T.Int; x = name }

  let make_basic_top normalty =
    OverTy_base { basename = nu; normalty; prop = P.True }

  let make_arrow argname argty rettyf =
    OverTy_arrow
      {
        argname;
        argty;
        retty =
          rettyf P.{ ty = basic_normalty_to_smtty @@ erase argty; x = argname };
      }

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
      | _ -> failwith "arrow_args_rename"
    in
    aux args overftp

  let is_base_type = function OverTy_base _ -> true | _ -> false
end
