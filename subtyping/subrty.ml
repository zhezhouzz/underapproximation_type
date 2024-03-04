open Languagez

(* open Frontendz *)
(* open Zzdatatype.Datatype *)
open Sugar

let rec sub_rty_bool rctx (rty1, rty2) =
  match (rty1, rty2) with
  | RtyBase { ou = true; cty = cty1 }, RtyBase { ou = true; cty = cty2 } ->
      Subcty.sub_cty_bool rctx (cty1, cty2)
  | RtyBase { ou = false; cty = cty1 }, RtyBase { ou = false; cty = cty2 } ->
      Subcty.sub_cty_bool rctx (cty2, cty1)
  | ( RtyBaseArr { argcty = argcty1; arg = arg1; retty = retty1 },
      RtyBaseArr { argcty = argcty2; arg = arg2; retty = retty2 } ) ->
      Subcty.sub_cty_bool rctx (argcty2, argcty1)
      &&
      let retty2 =
        subst_rty_instance arg2 (AVar arg1 #: (erase_cty argcty1)) retty2
      in
      sub_rty_bool rctx (retty1, retty2)
  | ( RtyArrArr { argrty = argrty1; retty = retty1 },
      RtyArrArr { argrty = argrty2; retty = retty2 } ) ->
      sub_rty_bool rctx (argrty2, argrty1) && sub_rty_bool rctx (retty1, retty2)
  | _, _ -> _failatwith __FILE__ __LINE__ "die"

let is_under_bot_rty rctx = function
  | RtyBase { ou = false; cty } -> Subcty.is_bot_cty rctx cty
  | _ -> _failatwith __FILE__ __LINE__ "die"
