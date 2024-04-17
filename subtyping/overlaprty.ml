open Language
open Rctx

(* open Zzdatatype.Datatype *)
open Sugar

let _overlap_rty_opt rctx (rty1, rty2) =
  let rec aux rctx (rty1, rty2) =
    (* let () = *)
    (*   Printf.printf "rty1: %s --- rty2: %s\n" (layout_rty rty1) *)
    (*     (layout_rty rty2) *)
    (* in *)
    match (rty1, rty2) with
    | RtyBase { ou = Ex; cty = cty1; er }, RtyBase { ou = Fa; cty = cty2; _ } ->
        let* cty =
          Subcty.is_nonempty_cty_opt rctx (intersect_ctys [ cty1; cty2 ])
        in
        Some (RtyBase { ou = Ex; cty; er })
    | ( RtyBaseArr { argcty = argcty1; arg = arg1; retty = retty1 },
        RtyBaseArr { argcty = argcty2; arg = arg2; retty = retty2 } ) ->
        let* argcty =
          Subcty.is_nonempty_cty_opt rctx @@ intersect_ctys [ argcty1; argcty2 ]
        in
        let retty2 =
          subst_rty_instance arg2 (AVar arg1 #: (erase_cty argcty1)) retty2
        in
        let rctx =
          add_to_right rctx
            arg1 #: (RtyBase { ou = Fa; cty = argcty; er = mk_false })
        in
        let* retty = aux rctx (retty1, retty2) in
        Some (RtyBaseArr { argcty; arg = arg1; retty })
    | ( RtyBaseDepPair { argcty = argcty1; arg = arg1; retty = retty1 },
        RtyBaseArr { argcty = argcty2; arg = arg2; retty = retty2 } ) ->
        if Subcty.sub_cty_bool rctx (argcty1, argcty2) then
          let retty2 =
            subst_rty_instance arg2 (AVar arg1 #: (erase_cty argcty1)) retty2
          in
          let rctx =
            add_to_right rctx
              arg1 #: (RtyBase { ou = Ex; cty = argcty1; er = mk_false })
          in
          let* retty = aux rctx (retty1, retty2) in
          Some (RtyBaseDepPair { argcty = argcty1; arg = arg1; retty })
        else None
    | ( RtyArrArr { argrty = argrty1; retty = retty1 },
        RtyArrArr { argrty = argrty2; retty = retty2 } ) ->
        let* argrty = aux rctx (argrty1, argrty2) in
        let* retty = aux rctx (retty1, retty2) in
        Some (RtyArrArr { argrty; retty })
    | _, _ ->
        let () =
          Printf.printf "%s ⋐ %s\n" (layout_rty rty1) (layout_rty rty2)
        in
        _failatwith __FILE__ __LINE__ "die"
  in
  aux rctx (rty1, rty2)

let overlap_rty_opt = _overlap_rty_opt

let overlap_rty_bool rtcx (rty1, rty2) =
  (* let rty1, rty2 = map2 _desugar_rty_ret_under (rty1, rty2) in *)
  (* let () = Printf.printf "Desugar:\n" in *)
  (* let () = Printf.printf "%s <: %s\n" (layout_rty rty1) (layout_rty rty2) in *)
  match overlap_rty_opt rtcx (rty1, rty2) with Some _ -> true | None -> false

let external_check ctx (rty1, rty2) =
  let () = Printf.printf "Overlaping:\n" in
  let () = Printf.printf "%s ⋐ %s\n" (layout_rty rty1) (layout_rty rty2) in
  let rty = overlap_rty_opt ctx (rty1, rty2) in
  let () =
    Tyctx.pprint_typectx_overlaptyping (fun _ -> pprint_typectx emp) (rty1, rty2)
  in
  let () = Printf.printf "Result: %s\n" (opt_layout layout_rty rty) in
  ()
