open Lang
open Typedlang

(* open Sugar *)
open Feature

type t = Nt.t

let cegis features verifier =
  let layout_vec_id fv = layout_prop @@ feature_id_to_prop features fv in
  let fvec_tab = Fvec_tab.init features in
  let rec loop () =
    match Fvec_tab.pick_unknown fvec_tab with
    | Some fv ->
        let () =
          Env.show_debug_queries @@ fun _ ->
          Pp.printf "@{<bold>@{<orange>\tpick_maybepos: %s@}@}\n"
            (layout_vec_id fv)
        in
        let () = Fvec_tab.update fvec_tab fv Neg in
        let candidate = Fvec_tab.get_candidate fvec_tab in
        if verifier candidate then
          let () =
            Env.show_debug_queries @@ fun _ ->
            Pp.printf "@{<bold>@{<orange>\tlabel %s as - @}@}\n"
              (layout_vec_id fv)
          in
          loop ()
        else
          let () =
            Env.show_debug_queries @@ fun _ ->
            Pp.printf "@{<bold>@{<orange>\tlabel %s as + @}@}\n"
              (layout_vec_id fv)
          in
          Fvec_tab.update fvec_tab fv Pos;
          loop ()
    | None ->
        let solution = Fvec_tab.get_candidate fvec_tab in
        if verifier solution then Some solution else None
  in
  let res = loop () in
  res
