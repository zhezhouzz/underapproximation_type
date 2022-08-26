open Sugar
open Prim_map

let init (type_decls, normals, overs, unders, rev_unders) =
  let nm = make_normal type_decls normals in
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

let get_primitive_over_ty name =
  let _, entry = get_by_name (get_notation_m ()) name in
  match entry.overty with
  | None ->
      _failatwith __FILE__ __LINE__
      @@ spf "cannot find built-in refinement type (%s)" name
  | Some x -> x

let get_primitive_under_ty name =
  let _, entry = get_by_name (get_notation_m ()) name in
  match entry.qunderty with
  | None ->
      _failatwith __FILE__ __LINE__
      @@ spf "cannot find built-in under type (%s)" name
  | Some x -> x

let get_primitive_rev_under_ty name =
  let _, entry = get_by_name (get_notation_m ()) name in
  match entry.rev_qunderty with
  | None ->
      _failatwith __FILE__ __LINE__
      @@ spf "cannot find built-in rev under type (%s)" name
  | Some x -> x

let normal_check_if_is_known_prims name =
  try Some (get_by_name (get_normal_m ()) name) with _ -> None
