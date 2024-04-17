open Language
(* open Checkaux *)

(* open Rctx *)
open Sugar
open Zzdatatype.Datatype

type check_result = ResBool of bool | ResRty of t rty option

type check_mode =
  | TypeCheck
  | TypeInfer
  | TypeRefine
  | TypeRefineCodomain
  | TypeRefineDomain
  | TypeRefineBi

let layout_check_mode = function
  | TypeCheck -> "type check"
  | TypeInfer -> "type infer"
  | TypeRefine -> "type refine"
  | TypeRefineCodomain -> "type refine codomain"
  | TypeRefineDomain -> "type refine domain"
  | TypeRefineBi -> "type refine domain and codomain"

let layout_check_res = function
  | ResBool _ -> ""
  | ResRty rty -> opt_layout layout_rty rty

let get_check_result_bool = function
  | ResBool b -> b
  | ResRty None -> false
  | ResRty (Some _) -> true

let item_check (mode : check_mode) (axioms, uctx) imps = function
  | MFuncImp { name; body; _ } ->
      (* let body = term_to_value body in *)
      Some (uctx, StrMap.add name.x body imps)
  | MRty { is_assumption = true; name; rty } ->
      Some (add_to_right uctx name #: rty, imps)
  | MRty { is_assumption = false; name; rty } ->
      let imp =
        match StrMap.find_opt imps name with
        | None ->
            _failatwith __FILE__ __LINE__
              (spf "The source code of given refinement type '%s' is missing."
                 name)
        | Some v -> v
      in
      let () =
        Env.show_debug_typing @@ fun _ ->
        Pp.printf "@{<bold>Type Check %s:@}\n" name
      in
      let () =
        Env.show_debug_typing @@ fun _ ->
        Pp.printf "@{<bold>check against with:@} %s\n" (layout_rty rty)
      in
      let _ = Nt._type_unify __FILE__ __LINE__ imp.ty (erase_rty rty) in
      let rctx = Rctx.{ builtin_ctx = uctx; local_ctx = emp; axioms } in
      (* let rty = alpha_renaming_rty_term rctx imp rty in *)
      let res =
        match mode with
        | TypeCheck ->
            let res =
              match Termcheck.term_type_check rctx imp rty with
              | None -> false
              | Some _ -> true
            in
            ResBool res
        | TypeRefine -> ResRty (Termrefine.term_type_refine rctx imp rty)
        | _ -> _failatwith __FILE__ __LINE__ "unimp"
      in
      if get_check_result_bool res then (
        ( Env.show_debug_typing @@ fun _ ->
          Pp.printf "@{<bold>@{<yellow>Task %s, %s successed:@}@}\n\t%s\n" name
            (layout_check_mode mode) (layout_check_res res) );
        Some (add_to_right uctx name #: rty, imps))
      else (
        ( Env.show_debug_typing @@ fun _ ->
          Pp.printf "@{<bold>@{<red>Task %s, %s failed.@}@}\n" name
            (layout_check_mode mode) );
        None)
  | _ -> Some (uctx, imps)

let gather_uctx l =
  let l =
    List.filter_map
      (function
        | MRty { is_assumption = true; name; rty } -> Some name #: rty
        | _ -> None)
      l
  in
  add_to_rights emp l

let gather_props l =
  let l =
    List.filter_map
      (function MAxiom { name; prop } -> Some (name, prop) | _ -> None)
      l
  in
  l

let struc_check mode (axioms, uctx) items =
  let res =
    List.fold_left
      (fun res item ->
        let* uctx, imps = res in
        item_check mode (axioms, uctx) imps item)
      (Some (uctx, StrMap.empty))
      items
  in
  match res with Some _ -> true | None -> false
