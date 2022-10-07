open Sugar

module F (Type : Type.T) = struct
  open Sexplib.Std

  type ctx = (string * Type.t) list [@@deriving sexp]

  let get_opt ctx id = List.find_opt (fun (y, _) -> String.equal y id) ctx

  let get_ty ctx id =
    match get_opt ctx id with
    | None -> failwith "Not found"
    | Some (_, ty) -> ty

  let empty = []
  let filter_map = List.filter_map
  let fold_right = List.fold_right

  let exists ctx name =
    List.exists (fun (name', _) -> String.equal name name') ctx

  let add_to_right ctx (name, ty) =
    if exists ctx name then _failatwith __FILE__ __LINE__ ""
    else ctx @ [ (name, ty) ]

  let add_to_rights ctx l =
    List.fold_left (fun ctx x -> add_to_right ctx x) ctx l
end

module NSimpleTypectx = struct
  include F (Normalty.Ast.T)

  let of_type_decls e : ctx = Type_dec.T.mk_ctx e
end

module SMTSimpleTypectx = F (Normalty.Ast.Smtty)

module UTSimpleTypectx = struct
  include F (Underty.T)
  open Underty.T

  let subtract ctx ctx' =
    let rec aux = function
      | ctx, [] -> ctx
      | [], _ -> _failatwith __FILE__ __LINE__ ""
      | (name, ty) :: ctx, (name', ty') :: ctx' ->
          if String.equal name name' && eq ty ty' then aux (ctx, ctx')
          else _failatwith __FILE__ __LINE__ ""
    in
    aux (ctx, ctx')

  let destrct_right ctx =
    match List.rev ctx with [] -> None | h :: t -> Some (List.rev t, h)
end

module OverTypectx = F (Overty.T)
module MustMayTypectx = F (Underty.MMT)
