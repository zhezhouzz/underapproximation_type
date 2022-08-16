open Zzdatatype.Datatype
module NT = Languages.Normalty
module UT = Languages.Underty
module QUT = Languages.Qunderty
module Op = Languages.Op
module P = Autov.Prop
module T = Autov.Smtty

(* let rev_tab_names = [ ("rev_intlistnil", "[]"); ("rev_intlistcons", "::") ] *)
let m : Languages.Qunderty.t StrMap.t option ref = ref None
let rev_m : Languages.Qunderty.t StrMap.t option ref = ref None

let make_key (name, QUT.{ qbody = t; _ }) =
  try
    let op = Op.op_of_alias name in
    let ty = UT.erase t in
    Op.PrimOp (op, ty)
  with _ -> Op.External name

let make_m m (refinements : (string * Languages.Qunderty.t) list) =
  let l = List.map (fun (name, ty) -> (make_key (name, ty), ty)) refinements in
  m :=
    Some
      (StrMap.from_kv_list
      @@ List.map
           (fun (k, v) -> (Core.Sexp.to_string @@ Op.sexp_of_prim k, v))
           l)

(* let tab, rev_tab, rest = *)
(*   List.fold_left *)
(*     (fun (m, rev_m, rest) (x, ty) -> *)
(*       match List.find_opt (fun (idx, _) -> String.equal idx x) tab_names with *)
(*       | Some (_, name) -> ((name, ty) :: m, rev_m, rest) *)
(*       | None -> ( *)
(*           match *)
(*             List.find_opt (fun (idx, _) -> String.equal idx x) rev_tab_names *)
(*           with *)
(*           | Some (_, name) -> (m, (name, ty) :: rev_m, rest) *)
(*           | None -> (m, rev_m, (x, ty) :: rest))) *)
(*     ([], [], []) refinements *)
(* in *)
(* let check tab m = *)
(*   List.iter *)
(*     (fun (_, name) -> *)
(*       match List.find_opt (fun (x, _) -> String.equal name x) m with *)
(*       | None -> failwith "cannot find under type of built-in prim" *)
(*       | Some _ -> ()) *)
(*     tab *)
(* in *)
(* let () = check tab_names tab in *)
(* let () = check rev_tab_names rev_tab in *)
(* m := Some (StrMap.from_kv_list @@ tab @ rest); *)
(* rev_m := Some (StrMap.from_kv_list rev_tab) *)
