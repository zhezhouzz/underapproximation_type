module S = Map.Make (Languages.Op)
open Languages.Op
module NT = Languages.Normalty

let typed_prim_of_string ctx = function
  | "[]" ->
      let x = DtConstructor "nil" in
      (x, S.find_opt x ctx)
  | "::" ->
      let x = DtConstructor "cons" in
      (x, S.find_opt x ctx)
  | name -> (
      match op_of_string_opt name with
      | Some op ->
          let x = PrimOp op in
          (x, S.find_opt x ctx)
      | None -> (
          let x = DtConstructor name in
          match S.find_opt x ctx with
          | Some ty -> (x, Some ty)
          | None ->
              let x = External name in
              (x, S.find_opt x ctx)))

let make_normal type_decls (normals : (string * NT.t) list) =
  let kvs =
    List.map (fun (name, ty) -> (DtConstructor name, ty))
    @@ Languages.NSimpleTypectx.of_type_decls type_decls
  in
  let m = S.of_seq @@ List.to_seq kvs in
  let m =
    List.fold_left
      (fun m (name, ty) ->
        match name with
        | "nil" | "cons" -> S.add (DtConstructor name) ty m
        | name -> (
            match op_of_alias_opt name with
            | Some op -> S.add (PrimOp op) ty m
            | None -> S.add (External name) ty m))
      m normals
  in
  m

module QUT = Languages.Qunderty
module OT = Languages.Overty

type notation = {
  overty : OT.t option;
  qunderty : QUT.t option;
  rev_qunderty : QUT.t option;
}

open Zzdatatype.Datatype

let consume prim (m : 'a StrMap.t) : 'a option * 'a StrMap.t =
  let name = t_to_string_for_load prim in
  match StrMap.find_opt m name with
  | None -> (None, m)
  | Some ty -> (Some (ty : 'a), StrMap.remove name m)

open Sugar

let layout_m m =
  S.iter
    (fun name _ -> Printf.printf "key: %s\n" @@ t_to_string_for_load name)
    m

let make_m normal_m (over_refinements : (string * OT.t) list)
    (under_refinements : (string * QUT.t) list)
    (rev_under_refinements : (string * QUT.t) list) =
  let om = StrMap.from_kv_list over_refinements in
  let um = StrMap.from_kv_list under_refinements in
  let rum = StrMap.from_kv_list rev_under_refinements in
  let make_one prim (om, um, rum) =
    let overty, om = consume prim om in
    let qunderty, um = consume prim um in
    let rev_qunderty, rum = consume prim rum in
    ({ overty; qunderty; rev_qunderty }, (om, um, rum))
  in
  let m, om, um, rum =
    S.fold
      (fun prim _ (res, om, um, rum) ->
        let entry, (om, um, rum) = make_one prim (om, um, rum) in
        (S.add prim entry res, om, um, rum))
      normal_m (S.empty, om, um, rum)
  in
  let check m =
    StrMap.iter
      (fun name _ ->
        _failatwith __FILE__ __LINE__
          (spf "provided type of %s but cannot be loaded" name))
      m
  in
  check om;
  check um;
  check rum;
  m

let normal_m : NT.t S.t option ref = ref None
let notation_m : notation S.t option ref = ref None

let get_normal_m () =
  match !normal_m with
  | None -> _failatwith __FILE__ __LINE__ "un init"
  | Some m -> m

let get_notation_m () =
  match !notation_m with
  | None -> _failatwith __FILE__ __LINE__ "un init"
  | Some m -> m

let get_by_name m name =
  match typed_prim_of_string m name with
  | _, None ->
      let () = layout_m m in
      _failatwith __FILE__ __LINE__ @@ spf "cannot find prim %s" name
  | prim, Some x -> (prim, x)
