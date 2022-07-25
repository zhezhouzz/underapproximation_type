open Zzdatatype.Datatype

let get_primitive_ty m name =
  StrMap.find (Sugar.spf "cannot find primitive type of %s" name) m name

let get_primitive_normal_ty = get_primitive_ty Normalprim.m
let init_over_prim p = Overprim.make_m p

let get_primitive_over_ty name =
  match !Overprim.m with
  | None -> failwith "uninited get_primitive_under_ty"
  | Some m -> get_primitive_ty m name

let init_under_prim p = Underprim.make_m p

let get_primitive_under_ty name =
  match !Underprim.m with
  | None -> failwith "uninited get_primitive_under_ty"
  | Some m ->
      (* let () = *)
      (*   Printf.printf "finding in prim:\n%s\n" *)
      (*   @@ List.to_string (fun (name, ty) -> *)
      (*          Printf.sprintf "%s: %s;" name (Frontend.Undertype.layout ty)) *)
      (*   @@ StrMap.to_kv_list m *)
      (* in *)
      get_primitive_ty m name

let get_primitive_rev_under_ty name =
  match !Underprim.rev_m with
  | None -> failwith "uninited get_primitive_rev_under_ty"
  | Some m -> get_primitive_ty m name
