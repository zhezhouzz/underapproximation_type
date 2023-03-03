open Ast
module S = Map.Make (Op)
open Op
module Type = Normalty.Ast.T

let typed_prim_of_string ctx = function
  | "[]" ->
      let x = DtConstructor "nil" in
      (x, S.find_opt x ctx)
  | "::" ->
      let x = DtConstructor "cons" in
      (x, S.find_opt x ctx)
  | "()" ->
      let x = DtConstructor "tt" in
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
    @@ NSimpleTypectx.of_type_decls type_decls
  in
  let m = S.of_seq @@ List.to_seq kvs in
  let m =
    List.fold_left
      (fun m (name, ty) ->
        match name with
        | "nil" | "cons" | "tt" -> S.add (DtConstructor name) ty m
        | name -> (
            match op_of_alias_opt name with
            | Some op -> S.add (PrimOp op) ty m
            | None -> S.add (External name) ty m))
      m normals
  in
  m

type notation = {
  overty : OT.t list;
  qunderty : UT.t list;
  rev_qunderty : UT.t list;
}

open Zzdatatype.Datatype

let consume prim (m : (string * 'a) list) : 'a list * (string * 'a) list =
  let name = t_to_string_for_load prim in
  let l, m = List.partition (fun (name', _) -> String.equal name name') m in
  let l = List.map snd l in
  (l, m)

open Sugar

let layout_m m =
  S.iter
    (fun name _ ->
      Env.show_debug_debug (fun _ ->
          Printf.printf "key: %s\n" @@ t_to_string_for_load name))
    m

let safe_make_m l =
  let m = StrMap.from_kv_list l in
  if List.length l != StrMap.cardinal m then
    _failatwith __FILE__ __LINE__ "duplicate names"
  else m

let make_m normal_m (over_refinements : (string * OT.t) list)
    (under_refinements : (string * UT.t) list)
    (rev_under_refinements : (string * UT.t) list) =
  (* let om = safe_make_m over_refinements in *)
  (* let um = safe_make_m under_refinements in *)
  (* let rum = safe_make_m rev_under_refinements in *)
  let om = over_refinements in
  let um = under_refinements in
  let rum = rev_under_refinements in
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
  let check m : unit =
    List.iter
      (fun (name, _) ->
        let () = layout_m normal_m in
        _failatwith __FILE__ __LINE__
          (spf "provided type of %s but cannot be loaded" name))
      m
  in
  check om;
  check um;
  check rum;
  m

let make_lemmas = safe_make_m
let lemma_m : Lemma.t StrMap.t option ref = ref None
let functional_lemma_m : Lemma.t StrMap.t option ref = ref None
let normal_m : NT.t S.t option ref = ref None
let notation_m : notation S.t option ref = ref None

let lemmas_to_pres () =
  match !lemma_m with
  | None -> _failatwith __FILE__ __LINE__ "un init"
  | Some m -> StrMap.to_value_list m

let functional_lemmas_to_pres () =
  match !functional_lemma_m with
  | None -> _failatwith __FILE__ __LINE__ "un init"
  | Some m -> StrMap.to_value_list m

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
      (* let () = layout_m m in *)
      _failatwith __FILE__ __LINE__ @@ spf "cannot find prim %s" name
  | prim, Some x -> (prim, x)
