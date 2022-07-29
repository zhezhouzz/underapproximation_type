module NL = Languages.NormalAnormal
module UL = Languages.UnderAnormal
module NT = Languages.Normalty
module UT = Languages.Underty
open Zzdatatype.Datatype
open Sugar

let layout_subtyping = Frontend.Typectx.pretty_layout_under_subtyping

let simply_ctx ctx fv =
  let in_fv fv name = List.exists (fun x -> String.equal x name) fv in
  let open UT in
  let rec aux fv = function
    | [] -> []
    | (name, ty) :: ctx ->
        (* let () = *)
        (*   Printf.printf "\tfind %s in %s\n" name (StrList.to_string fv) *)
        (* in *)
        if in_fv fv name then
          match ty with
          | UnderTy_base { basename; prop; normalty } ->
              let prop = P.subst_id prop basename name in
              let fv' = Autov.add_prop_to_fv fv prop in
              (name, normalty, prop) :: aux fv' ctx
          | _ -> _failatwith __FILE__ __LINE__ "should not happen"
        else aux fv ctx
  in
  List.rev @@ aux fv (List.rev ctx)

(* TODO: well founded check *)
let subtyping_to_query ctx typeself (prop1, prop2) =
  let fv_prop2 = Autov.add_prop_to_fv [ typeself ] prop2 in
  let fv = Autov.add_prop_to_fv fv_prop2 prop1 in
  let pre_common, pre_fv1 =
    List.partition (fun (x, _, _) -> List.exists (String.equal x) fv_prop2)
    @@ simply_ctx ctx fv
  in
  let open Autov.Prop in
  let p2 = prop2 in
  let p1 =
    List.fold_right
      (fun (x, _, xprop) prop ->
        mk_exists_intqv x (fun _ -> And [ xprop; prop ]))
      pre_fv1 prop1
  in
  let q =
    List.fold_right
      (fun (x, _, xprop) prop ->
        mk_forall_intqv x (fun _ -> Implies (xprop, prop)))
      pre_common
      (Implies (p2, p1))
  in
  let () = Printf.printf "SMT check:\n%s\n" (Autov.pretty_layout_prop q) in
  q

let subtyping_check (ctx : UT.t Typectx.t) (t1 : UT.t) (t2 : UT.t) =
  let open UT in
  let rec aux ctx (t1, t2) =
    let () = Printf.printf "Subtype: %s\n" @@ layout_subtyping ctx (t1, t2) in
    match (t1, t2) with
    | ( UnderTy_base { basename = name1; prop = prop1; _ },
        UnderTy_base { basename = name2; prop = prop2; _ } ) ->
        let typeself, prop1, prop2 =
          match (Typectx.in_ctx ctx name1, Typectx.in_ctx ctx name2) with
          | true, true ->
              ( _check_equality __FILE__ __LINE__ String.equal name1 name2,
                prop1,
                prop2 )
          | false, true -> (name2, P.subst_id prop1 name1 name2, prop2)
          | _, _ -> (name1, prop1, P.subst_id prop2 name2 name1)
        in
        let q = subtyping_to_query ctx typeself (prop1, prop2) in
        if Autov.check q then ()
        else failwith "Subtyping check: rejected by the verifier"
    | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
        List.iter (aux ctx) @@ List.combine ts1 ts2
    | ( UnderTy_arrow { argname = x1; argty = t11; retty = t12 },
        UnderTy_arrow { argname = x2; argty = t21; retty = t22 } ) ->
        let t22 = subst_id t22 x2 x1 in
        let () = aux ctx (t21, t11) in
        let () = aux ctx (t12, t22) in
        ()
    | _, _ -> _failatwith __FILE__ __LINE__ "die: under subtype"
  in
  aux ctx (t1, t2)
