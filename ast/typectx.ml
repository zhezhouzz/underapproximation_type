module F (R : Refinement.T) = struct
  open Zzdatatype.Datatype
  open Sexplib.Std
  open Sugar

  type t = (string * R.t list) list [@@deriving sexp]

  let empty = []
  let add_to_left (ty, name) ctx = (name, [ ty ]) :: ctx
  let exists ctx name = List.exists (fun (x, _) -> String.equal x name) ctx

  let get_opt (ctx : t) id : 'a option =
    let* _, t = List.find_opt (fun (id', _) -> String.equal id id') ctx in
    Some (R.conjunct_list t)

  let get_ty (ctx : t) id : 'a =
    match get_opt ctx id with
    | None -> failwith @@ Sugar.spf "no such name (%s) in the type context" id
    | Some ty -> ty

  let add_to_right ctx (ty, id) =
    if exists ctx id then _failatwith __FILE__ __LINE__ (spf "Add %s" id)
    else ctx @ [ (id, [ ty ]) ]

  let add_to_rights ctx l = List.fold_left add_to_right ctx l

  let rec subtract_inner = function
    | l, [] -> l
    | [], _ -> _failatwith __FILE__ __LINE__ ""
    | x1 :: t1, x2 :: t2 ->
        if R.eq x1 x2 then subtract_inner (t1, t2)
        else _failatwith __FILE__ __LINE__ ""

  let subtract ctx ctx' =
    let rec aux = function
      | l, [] -> List.map (fun x -> (true, x)) l
      | [], _ -> _failatwith __FILE__ __LINE__ ""
      | (x1, ys1) :: t1, (x2, ys2) :: t2 ->
          if String.equal x1 x2 then
            let diff = subtract_inner (ys1, ys2) in
            match diff with
            | [] -> aux (t1, t2)
            | diff -> (false, (x1, diff)) :: aux (t1, t2)
          else _failatwith __FILE__ __LINE__ ""
    in
    aux (ctx, ctx')

  let fold_right = List.fold_right
  let filter_map = List.filter_map

  let fv l =
    let fv =
      List.concat @@ List.map (fun (id, t) -> id :: List.concat_map R.fv t) l
    in
    List.slow_rm_dup String.equal fv

  let var_space (l : t) =
    let fv =
      List.concat
      @@ List.map (fun (id, t) -> id :: List.concat_map R.var_space t) l
    in
    List.slow_rm_dup String.equal fv

  let conjunct ctx (id, ty) =
    let counter = ref 0 in
    let ctx =
      List.map
        (fun (x, tys) ->
          if String.equal x id then (
            counter := !counter + 1;
            (x, tys @ [ ty ]))
          else (x, tys))
        ctx
    in
    if !counter != 1 then failwith "type ctx update error" else ctx

  let subst_id ctx id id' =
    List.map
      (fun (x, tys) ->
        let x = if String.equal x id then id' else x in
        (x, List.map (fun ty -> R.subst_id ty id id') tys))
      ctx

  let eq ctx ctx' =
    try match subtract ctx ctx' with [] -> true | _ -> false with _ -> false
end

module UnderTypectx = struct
  include F (Underty.T)
  open Underty.T
  open Normalty.Ast.Ntyped
  open Sugar
  module UL = Anormal.UnderAnormal

  let add_to_right ctx UL.{ ty; x } = add_to_right ctx (ty, x)

  let add_hidden_vars_to_right ctx (hvs, ty) =
    let hvs, ty =
      List.fold_left
        (fun (hvs, ty) x ->
          let x' = { x = Rename.unique x.x; ty = x.ty } in
          (hvs @ [ x' ], subst_id ty x.x x'.x))
        ([], ty) hvs
    in
    let hvs = List.map (fun x -> (make_basic_top x.ty, x.x)) hvs in
    (add_to_rights ctx hvs, ty)

  let add_to_rights ctx l = List.fold_left add_to_right ctx l

  let add_arrow_arg_to_right ctx (hidden_vars, id) =
    let ctx, idty = add_hidden_vars_to_right ctx (hidden_vars, id.UL.ty) in
    add_to_right ctx { x = id.x; ty = idty }

  let get_nt = function
    | [] -> _failatwith __FILE__ __LINE__ "never happen"
    | h :: _ -> erase h

  let extract_vars_by_nt ctx nt =
    List.filter_map
      (fun (x, tys) ->
        let nt' = get_nt tys in
        if eq nt nt' then Some x else None)
      ctx

  let destrct_right ctx =
    match List.rev ctx with [] -> None | h :: t -> Some (List.rev t, h)
end

module OverTypectx = F (Overty.T)
