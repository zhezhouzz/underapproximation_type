open Languages
module P = Autov.Prop
module Typectx = UnderTypectx
open Zzdatatype.Datatype

(* open Sugar *)
open Ntyped
open UT

let reduction t =
  let () =
    Env.show_debug_tying @@ fun _ ->
    Pp.printf "@{<bold>Well Founded Check:@} %s\n" (UT.pretty_layout t)
  in
  let rec aux pres t =
    match t with
    | UnderTy_base { basename; normalty; prop } -> (
        match pres with
        | [] -> t
        | _ -> (
            let nu = { x = basename; ty = normalty } in
            let qvs', prop' =
              List.fold_right
                (fun (x, xprop) (qvs, prop') ->
                  if List.exists (String.equal x.x) (P.fv prop') then
                    (x :: qvs, P.And [ xprop; prop' ])
                  else (qvs, prop'))
                pres ([], prop)
            in
            let query = P.(topu_to_prop (nu :: qvs', Implies (prop, prop'))) in
            (* let () = *)
            (*   Printf.printf "Q: %s\n" @@ Autov.pretty_layout_prop query *)
            (* in *)
            match Autov.check [] query with
            | None -> t
            | Some _ ->
                let t' = UnderTy_base { basename; normalty; prop = prop' } in
                let () =
                  Env.show_debug_tying @@ fun _ ->
                  Pp.printf "@{<bold>Well Founded Reduction... @}\n"
                in
                let () =
                  Env.show_debug_tying @@ fun _ ->
                  Pp.printf "%s @{<bold>=>@} %s\n" (UT.pretty_layout t)
                    (UT.pretty_layout t')
                in
                t'))
    | UnderTy_tuple ts -> UnderTy_tuple (List.map (aux pres) ts)
    | UnderTy_poly_arrow { argname; argnty; retty } ->
        UnderTy_poly_arrow { argname; argnty; retty = aux pres retty }
    | UnderTy_arrow { argname; argty; retty } ->
        let () =
          Env.show_debug_tying @@ fun _ ->
          Pp.printf "@{<bold>Well Founded Check:@} %s\n"
            (UT.pretty_layout argty)
        in
        (* let argty = aux (eqvs, props) argty in *)
        let retty =
          (* if List.exists (String.equal argname) (fv retty) then *)
          let x, ty, xprop = assume_base __FILE__ __LINE__ argty in
          let xprop = P.subst_id xprop x argname in
          aux (pres @ [ ({ x = argname; ty }, xprop) ]) retty
          (* else aux (eqvs, props) retty *)
        in
        UnderTy_arrow { argname; argty; retty }
  in
  aux [] t
