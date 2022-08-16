open Zzdatatype.Datatype
module Op = Languages.Op

let init (overs, unders, rev_unders) =
  Overprim.(make_m m overs);
  Underprim.(make_m m unders);
  Underprim.(make_m rev_m rev_unders);
  Normalprim.make_m Overprim.m

let get m prim =
  let name = Core.Sexp.to_string @@ Op.sexp_of_prim prim in
  match !m with
  | None -> failwith "uninited get_primitive_rev_under_ty"
  | Some m ->
      (* let _ = StrMap.iter (fun prim _ -> Printf.printf "prim: %s\n" prim) m in *)
      let ty =
        StrMap.find (Sugar.spf "cannot find primitive type of %s" name) m name
      in
      (* let _ = Printf.printf "end\n" in *)
      ty

let get_primitive_normal_ty = get Normalprim.m

let _get_primitive_dt_normal_ty (name, tyeq) =
  match !Normalprim.m with
  | None -> failwith "uninited get_primitive_rev_under_ty"
  | Some m -> (
      let l =
        List.map (fun (str, v) ->
            (Op.prim_of_sexp @@ Core.Sexp.of_string str, v))
        @@ StrMap.to_kv_list m
      in
      match
        List.find_opt
          (function
           | Op.(PrimOp (Dt name', nt), _) -> String.equal name name' && tyeq nt
           | _ -> false
            : Op.prim * Normalty.T.t -> bool)
          (l : (Op.prim * Normalty.T.t) list)
      with
      | None -> failwith (Sugar.spf "cannot find primitive type of %s" name)
      | Some (_, nt) -> nt)

let get_primitive_dt_normal_ty (name, argsty) =
  let open Normalty.T in
  _get_primitive_dt_normal_ty
    ( name,
      fun nt ->
        let argsty', _ = destruct_arrow_tp nt in
        List.for_all2 eq argsty argsty' )

let get_primitive_dt_rev_normal_ty (name, retty) =
  let open Normalty.T in
  _get_primitive_dt_normal_ty
    ( name,
      fun nt ->
        let _, retty' = destruct_arrow_tp nt in
        eq retty retty' )

let get_primitive_over_ty = get Overprim.m
let get_primitive_under_ty = get Underprim.m
let get_primitive_rev_under_ty = get Underprim.rev_m
