open Zzdatatype.Datatype
module NT = Languages.Normalty
module Op = Languages.Op
open Sugar
(* let tab = *)
(*   let open Languages.Normalty in *)
(*   [ *)
(*     ("::", Ty_arrow (Ty_int, Ty_arrow (Ty_list Ty_int, Ty_list Ty_int))); *)
(*     ("[]", Ty_list Ty_int); *)
(*     ("<", Ty_arrow (Ty_int, Ty_arrow (Ty_int, Ty_bool))); *)
(*     (">", Ty_arrow (Ty_int, Ty_arrow (Ty_int, Ty_bool))); *)
(*     ("==", Ty_arrow (Ty_int, Ty_arrow (Ty_int, Ty_bool))); *)
(*     ("+", Ty_arrow (Ty_int, Ty_arrow (Ty_int, Ty_int))); *)
(*     ("-", Ty_arrow (Ty_int, Ty_arrow (Ty_int, Ty_int))); *)
(*   ] *)

(* let m = StrMap.from_kv_list tab *)

let m = ref None
let mk_external_name name = Core.Sexp.to_string @@ Op.sexp_of_prim name

let make_m type_decls under_m =
  let kvs =
    List.map (fun (name, ty) -> (mk_external_name (PrimOp (Op.Dt name)), ty))
    @@ Languages.NSimpleTypectx.of_type_decls type_decls
  in
  let open Languages.Qunderty in
  m :=
    match !under_m with
    | None -> _failatwith __FILE__ __LINE__ "uninit under prim"
    | Some m ->
        let m =
          StrMap.map (fun { qbody = t; _ } -> Languages.Underty.erase t) m
        in
        Some (StrMap.add_seq (List.to_seq kvs) m)

let check_if_is_known_ops name =
  match !m with
  | None -> _failatwith __FILE__ __LINE__ "uninit normal prim"
  | Some m -> (
      match Op.op_of_string_opt name with
      | Some x_op -> (
          match StrMap.find_opt m (mk_external_name (PrimOp x_op)) with
          | Some ty -> (x_op, ty)
          | None -> _failatwith __FILE__ __LINE__ "")
      | None -> (
          let x_op = Op.Dt name in
          (* let () = Printf.printf "start print\n" in *)
          (* let () = *)
          (*   StrMap.iter (fun name _ -> Printf.printf "key: %s\n" name) m *)
          (* in *)
          match StrMap.find_opt m (mk_external_name (PrimOp x_op)) with
          | Some ty -> (x_op, ty)
          | None -> _failatwith __FILE__ __LINE__ ""))

(* let get_primitive_ty name = *)
(*   match !m with *)
(*   | None -> failwith "uninit normal prim" *)
(*   | Some m -> *)
(*       StrMap.find (Sugar.spf "cannot find primitive type of %s" name) m name *)
