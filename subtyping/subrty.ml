open Language
open Rctx

(* open Zzdatatype.Datatype *)
open Sugar

let _sub_rty_bool rctx (rty1, rty2) =
  let rec aux rctx (rty1, rty2) =
    (* let () = *)
    (*   Printf.printf "rty1: %s --- rty2: %s\n" (layout_rty rty1) *)
    (*     (layout_rty rty2) *)
    (* in *)
    match (rty1, rty2) with
    | RtyGhostArr { argnty; arg; retty }, _ ->
        let rctx =
          add_to_right rctx
            arg
            #: (RtyBase { ou = Ex; cty = Cty { nty = argnty; phi = mk_true } })
        in
        aux rctx (retty, rty2)
    | _, RtyGhostArr { argnty; arg; retty } ->
        let rctx =
          add_to_right rctx
            arg
            #: (RtyBase { ou = Fa; cty = Cty { nty = argnty; phi = mk_true } })
        in
        aux rctx (rty1, retty)
    | RtyBase { ou = Fa; cty = cty1 }, RtyBase { ou = Fa; cty = cty2 } ->
        Subcty.sub_cty_bool rctx (cty1, cty2)
    | ( RtyBaseArr { argcty = argcty1; arg = arg1; retty = retty1 },
        RtyBaseArr { argcty = argcty2; arg = arg2; retty = retty2 } ) ->
        Subcty.sub_cty_bool rctx (argcty2, argcty1)
        &&
        let retty2 =
          subst_rty_instance arg2 (AVar arg1 #: (erase_cty argcty1)) retty2
        in
        let rctx =
          add_to_right rctx arg1 #: (RtyBase { ou = Fa; cty = argcty2 })
        in
        aux rctx (retty1, retty2)
    | ( RtyBaseDepPair { argcty = argcty1; arg = arg1; retty = retty1 },
        RtyBaseDepPair { argcty = argcty2; arg = arg2; retty = retty2 } ) ->
        Subcty.sub_cty_bool rctx (argcty1, argcty2)
        &&
        let arg2' =
          if String.equal arg1 arg2 then Rename.unique arg2 else arg2
        in
        let retty2 =
          subst_rty_instance arg2 (AVar arg2' #: (erase_cty argcty1)) retty2
        in
        let rctx =
          add_to_rights rctx
            [
              arg2' #: (RtyBase { ou = Fa; cty = argcty1 });
              arg1 #: (RtyBase { ou = Ex; cty = argcty1 });
            ]
        in
        aux rctx (retty1, retty2)
    | ( RtyArrArr { argrty = argrty1; retty = retty1 },
        RtyArrArr { argrty = argrty2; retty = retty2 } ) ->
        aux rctx (argrty2, argrty1) && aux rctx (retty1, retty2)
    | RtyInter (rty11, rty12), _ ->
        (* NOTE: safe, but find of weak, is there more complete solution? *)
        aux rctx (rty11, rty2) || aux rctx (rty12, rty2)
    | _, RtyInter (rty21, rty22) ->
        aux rctx (rty1, rty21) && aux rctx (rty1, rty22)
    | _, _ ->
        let () =
          Printf.printf "%s <: %s\n" (layout_rty rty1) (layout_rty rty2)
        in
        _failatwith __FILE__ __LINE__ "die"
  in
  aux rctx (rty1, rty2)

let sub_rty_bool rtcx (rty1, rty2) =
  _sub_rty_bool rtcx (_desugar_rty_ret_under rty1, _desugar_rty_ret_under rty2)

let is_nonempty_rty rctx = function
  | RtyBase { ou = Ex; cty } -> Subcty.is_nonempty_cty rctx cty
  | _ -> _failatwith __FILE__ __LINE__ "die"

let external_check ctx (rty1, rty2) =
  let () = Printf.printf "Subtyping:\n" in
  let () = Printf.printf "%s <: %s\n" (layout_rty rty1) (layout_rty rty2) in
  let rty1, rty2 = map2 _desugar_rty_ret_under (rty1, rty2) in
  let () = Printf.printf "Desugar:\n" in
  let () = Printf.printf "%s <: %s\n" (layout_rty rty1) (layout_rty rty2) in
  let res = sub_rty_bool ctx (rty1, rty2) in
  let () =
    Tyctx.pprint_typectx_subtyping (fun _ -> pprint_typectx emp) (rty1, rty2)
  in
  let () = Printf.printf "Result: %b\n" res in
  ()
