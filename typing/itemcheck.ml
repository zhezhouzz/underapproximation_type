open Language
open Sugar
open Zzdatatype.Datatype

let item_check (axioms, uctx) imps = function
  | MFuncImp { name; body; _ } ->
      let body = term_to_value body in
      Some (uctx, StrMap.add name.x body imps)
  | MRty { is_assumption = true; name; rty } ->
      Some (add_to_right uctx name #: rty, imps)
  | MRty { is_assumption = false; name; rty } -> (
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
        Pp.printf "@{<bold>check against with:@} %s\n"
          (FrontendTyped.layout_rty rty)
      in
      let _ = Nt._type_unify __FILE__ __LINE__ imp.ty (erase_rty rty) in
      match
        Termcheck.value_type_check_with_rec_check
          { builtin_ctx = uctx; local_ctx = emp; axioms }
          imp rty
      with
      | Some _ ->
          ( Env.show_debug_typing @@ fun _ ->
            Pp.printf "@{<bold>@{<yellow>Task %s, type check succeeded@}@}\n"
              name );
          Some (add_to_right uctx name #: rty, imps)
      | None ->
          ( Env.show_debug_typing @@ fun _ ->
            Pp.printf "@{<bold>@{<red>Task %s, type check failed@}@}\n" name );
          None)
  | _ -> Some (uctx, imps)

let item_infer (axioms, uctx) imps = function
  | MFuncImp { name; body; _ } ->
      let body = term_to_value body in
      Some (uctx, StrMap.add name.x body imps)
  | MRty { is_assumption = true; name; rty } ->
      Some (add_to_right uctx name #: rty, imps)
  | MRty { is_assumption = false; name; rty } -> (
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
        Pp.printf "@{<bold>Type partial infer %s:@}\n" name
      in
      let () =
        Env.show_debug_typing @@ fun _ ->
        Pp.printf "@{<bold>partial infer against with:@} %s\n"
          (FrontendTyped.layout_rty rty)
      in
      let _ = Nt._type_unify __FILE__ __LINE__ imp.ty (erase_rty rty) in
      match
        Termsyn.partial_value_type_infer
          { builtin_ctx = uctx; local_ctx = emp; axioms }
          imp rty
      with
      | Some _ ->
          ( Env.show_debug_typing @@ fun _ ->
            Pp.printf "@{<bold>@{<yellow>Task %s, type infer succeeded@}@}\n"
              name );
          Some (add_to_right uctx name #: rty, imps)
      | None ->
          ( Env.show_debug_typing @@ fun _ ->
            Pp.printf "@{<bold>@{<red>Task %s, type infer failed@}@}\n" name );
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

let gather_axioms l =
  let l =
    List.filter_map
      (function MAxiom { name; prop } -> Some name #: prop | _ -> None)
      l
  in
  l

let struc_check (axioms, uctx) items =
  let res =
    List.fold_left
      (fun res item ->
        let* uctx, imps = res in
        item_check (axioms, uctx) imps item)
      (Some (uctx, StrMap.empty))
      items
  in
  match res with Some _ -> true | None -> false

let struc_infer (axioms, uctx) items =
  let res =
    List.fold_left
      (fun res item ->
        let* uctx, imps = res in
        item_infer (axioms, uctx) imps item)
      (Some (uctx, StrMap.empty))
      items
  in
  match res with Some _ -> true | None -> false
