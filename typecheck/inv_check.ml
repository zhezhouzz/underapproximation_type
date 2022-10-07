module Nctx = Languages.UTSimpleTypectx
module Typectx = Languages.MustMayTypectx
open Zzdatatype.Datatype
module SNA = Languages.StrucNA
open Sugar
open Languages

let check (t1, t2) =
  let () =
    Pp.printf "@{<green>%s@} <: @{<yellow>%s@}\n" (UT.pretty_layout t1)
      (UT.pretty_layout t2)
  in
  let open UT in
  let _ = _check_equality __FILE__ __LINE__ NT.eq (erase t1) (erase t2) in
  let rec aux ctx (t1, t2) =
    match (t1, t2) with
    | UnderTy_base _, UnderTy_base _ ->
        Undersub.subtyping_check __FILE__ __LINE__ ctx t1 t2
    | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
        List.iter (aux ctx) @@ _safe_combine __FILE__ __LINE__ ts1 ts2
    | ( UnderTy_over_arrow { argname = argname1; argty = argty1; retty = retty1 },
        UnderTy_over_arrow
          { argname = argname2; argty = argty2; retty = retty2 } ) ->
        if String.equal argname1 argname2 && ot_strict_eq argty1 argty2 then
          let ctx = Typectx.ot_add_to_right ctx (argname1, argty1) in
          aux ctx (retty1, retty2)
        else _failatwith __FILE__ __LINE__ "unimp"
    | ( UnderTy_ghost_arrow
          { argname = argname1; argty = argty1; retty = retty1 },
        UnderTy_ghost_arrow
          { argname = argname2; argty = argty2; retty = retty2 } ) ->
        if String.equal argname1 argname2 && ot_strict_eq argty1 argty2 then
          let ctx = Typectx.ot_add_to_right ctx (argname1, argty1) in
          aux ctx (retty1, retty2)
        else _failatwith __FILE__ __LINE__ "unimp"
    | ( UnderTy_under_arrow { argty = argty1; retty = retty1 },
        UnderTy_under_arrow { argty = argty2; retty = retty2 } ) ->
        let () = aux ctx (argty2, argty1) in
        aux ctx (retty1, retty2)
    | _, _ -> _failatwith __FILE__ __LINE__ "should not happen"
  in
  aux [] (t1, t2)

let struc_check libs (name1, name2) =
  let libctx =
    List.fold_left
      (fun ctx (info, (name', ty)) -> StrMap.add name' (info, ty) ctx)
      StrMap.empty libs
  in
  let handle name1 =
    let info, r1 =
      StrMap.find
        (spf "Subtyping check: cannot find the type with name %s" name1)
        libctx name1
    in
    match info with
    | None -> r1
    | Some (name, _) ->
        let r1 = UT.reduce_inv_type_by_name r1 name in
        let () = Pp.printf "@{<bold>Close ghost arrow: %s@}\n" name in
        r1
  in
  let r1 = handle name1 in
  let r2 = handle name2 in
  try
    let _ = check (r1, r2) in
    let _ =
      Pp.printf
        "@{<bold>@{<yellow>Subtype Check %s <: %s, subtype check succeeded@}@}\n"
        name1 name2
    in
    ()
  with Autov.FailWithModel _ ->
    Pp.printf
      "@{<bold>@{<red>Subtype Check %s <: %s, subtype check failed@}@}\n" name1
      name2
