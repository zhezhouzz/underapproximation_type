open Sugar
open Prim_map

let init
    (type_decls, normals, overs, unders, rev_unders, lemmas, functional_lemmas)
    =
  let nm = make_normal type_decls normals in
  lemma_m := Some (make_lemmas lemmas);
  functional_lemma_m := Some (make_lemmas functional_lemmas);
  normal_m := Some nm;
  notation_m := Some (make_m nm overs unders rev_unders)

let get_primitive_normal_ty name =
  let _, ty = get_by_name (get_normal_m ()) name in
  ty

let get_primitive_dt_normal_ty (name, _) =
  let _, ty = get_by_name (get_normal_m ()) name in
  ty

let get_primitive_dt_rev_normal_ty (name, _) =
  let _, ty = get_by_name (get_normal_m ()) name in
  ty

let get_primitive__ msg m erase (name, ty) =
  match m with
  | [] ->
      _failatwith __FILE__ __LINE__
      @@ spf "cannot find built-in %s refinement type (%s)" msg name
  | l -> (
      let l = List.filter (fun ty' -> Normalty.Ast.T.eq (erase ty') ty) l in
      match l with
      | [] ->
          _failatwith __FILE__ __LINE__
          @@ spf
               "cannot find built-in %s refinement type (%s) with normal type \
                (%s)"
               msg name (Normalty.Ast.T.layout ty)
      | ty -> ty)

let get_primitive_ msg m erase (name, ty) =
  match get_primitive__ msg m erase (name, ty) with
  | [ ty ] -> ty
  | _ ->
      _failatwith __FILE__ __LINE__
      @@ spf
           "multiply types of built-in %s refinement type (%s) with normal \
            type (%s)"
           msg name (Normalty.Ast.T.layout ty)

let get_primitive_over_ty (name, ty) =
  let _, entry = get_by_name (get_notation_m ()) name in
  get_primitive_ "over" entry.overty Ast.OT.erase (name, ty)

let get_primitive_under_ty (name, ty) =
  let _, entry = get_by_name (get_notation_m ()) name in
  get_primitive_ "under" entry.qunderty Ast.UT.erase (name, ty)

let get_primitive_under_multi_ty (name, ty) =
  let _, entry = get_by_name (get_notation_m ()) name in
  get_primitive__ "under" entry.qunderty Ast.UT.erase (name, ty)

let get_primitive_rev_under_ty (name, ty) =
  let _, entry = get_by_name (get_notation_m ()) name in
  get_primitive__ "under rev" entry.rev_qunderty Ast.UT.erase (name, ty)

let normal_check_if_is_known_prims name =
  try Some (get_by_name (get_normal_m ()) name) with _ -> None

let lemmas_to_pres () = lemmas_to_pres ()
