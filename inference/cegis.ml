open Language
open FrontendTyped
open Sugar
open Feature

type t = Nt.t

let quick_return_interval = 3

let cegis features verifier =
  let layout_vec_id fv = layout_prop @@ feature_id_to_prop features fv in
  let fvec_tab = Fvec_tab.init features in
  let candidate = ref (mk_false, None) in
  let force_get_check_res () =
    match !candidate with
    | c, None ->
        let res = verifier c in
        candidate := (c, Some res);
        res
    | _, Some res -> res
  in
  let get_candidate () = fst !candidate in
  let counter = ref 0 in
  let rec loop () =
    let () = counter := !counter + 1 in
    if !counter mod quick_return_interval == 0 && force_get_check_res () then
      Some (get_candidate ())
    else
      match Fvec_tab.pick_unknown fvec_tab with
      | Some fv ->
          let () =
            Env.show_debug_queries @@ fun _ ->
            Pp.printf "@{<bold>@{<orange>\tpick_maybepos: %s@}@}\n"
              (layout_vec_id fv)
          in
          let () = Fvec_tab.update fvec_tab fv Neg in
          let candidate' = Fvec_tab.get_prop_by_filter is_not_neg fvec_tab in
          let () =
            if verifier candidate' then
              Env.show_debug_queries @@ fun _ ->
              Pp.printf "@{<bold>@{<orange>\tlabel %s as - @}@}\n"
                (layout_vec_id fv)
            else
              let () =
                Env.show_debug_queries @@ fun _ ->
                Pp.printf "@{<bold>@{<orange>\tlabel %s as + @}@}\n"
                  (layout_vec_id fv)
              in
              Fvec_tab.update fvec_tab fv Pos
          in
          candidate := (Fvec_tab.get_prop_by_filter is_positive fvec_tab, None);
          loop ()
      | None ->
          if force_get_check_res () then Some (get_candidate ())
          else _failatwith __FILE__ __LINE__ "die"
  in
  let res = loop () in
  res

(* let solution = Fvec_tab.get_candidate fvec_tab in *)
(* if verifier solution then Some solution else None *)
