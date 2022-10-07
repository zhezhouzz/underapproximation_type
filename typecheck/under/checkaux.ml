open Languages
open Sugar
module P = Autov.Prop

let check_empty_hidden file line hvs =
  if List.length hvs != 0 then
    _failatwith file line "do not allow hidden variables"
  else ()

open Zzdatatype.Datatype

let unify file line underfty normalty =
  let open UT in
  let open Ntyped in
  let rec aux m = function
    | UnderTy_base { basename; normalty; prop }, nt ->
        let m, normalty = NT._type_unify_ file line m normalty nt in
        let prop = P.type_update m prop in
        UnderTy_base { basename; normalty; prop }
    | UnderTy_over_arrow { argname; argty; retty }, Ty_arrow (t1, t2) ->
        let m, _ = NT._type_unify_ file line m argty.normalty t1 in
        let retty = aux m (retty, t2) in
        UnderTy_over_arrow { argname; argty; retty }
    | UnderTy_under_arrow { argty; retty }, Ty_arrow (t1, t2) ->
        let argty = aux m (argty, t1) in
        let retty = aux m (retty, t2) in
        UnderTy_under_arrow { argty; retty }
    | UnderTy_tuple uts, Ty_tuple ts ->
        let l = _safe_combine file line uts ts in
        UnderTy_tuple (List.map (aux m) l)
    | UnderTy_ghost_arrow { argname; argty; retty }, nt ->
        let retty = aux m (retty, nt) in
        UnderTy_ghost_arrow { argname; argty; retty }
    | _, _ -> _failatwith file line "unify"
  in
  aux StrMap.empty (underfty, normalty)

let erase_check_mk_id file line id underfty =
  (* let () = *)
  (*   Pp.printf "|_ %s _| ~> %s = %s\n" *)
  (*     (Frontend.Underty.layout underfty) *)
  (*     (Frontend.Type.layout @@ UT.erase underfty) *)
  (*     (Frontend.Type.layout id.NL.ty) *)
  (* in *)
  let ty = unify file line underfty (snd id.NL.ty) in
  UL.{ ty; x = id.x }
(* let _ = _check_equality file line NT.eq (UT.erase underfty) (snd id.NL.ty) in *)
(* UL.{ ty = underfty; x = id.x } *)

let subtyping_check = Undersub.subtyping_check
let subtyping_check_bool = Undersub.subtyping_check_bool

(* let subtyping_check__with_hidden_vars = *)
(*   Undersub.subtyping_check_with_hidden_vars *)

let merge_case_tys tys =
  (* let () = *)
  (*   List.iteri *)
  (*     (fun i ty -> *)
  (*       Pp.printf "@{<bold>Case(%i) ty@}: %s\n" i @@ UT.pretty_layout ty) *)
  (*     tys *)
  (* in *)
  let ty = UT.disjunct_list tys in
  (* let () = *)
  (*   Pp.printf "@{<bold>Merged ty@}: %s\n" @@ Frontend.Underty.pretty_layout ty *)
  (* in *)
  ty

(* let close_term_by_diff ctx' ctx UL.{ ty; x } = *)
(*   let _ = UL.{ x; ty = UnderTypectx.close_by_diff ctx' ctx ty } in *)
(*   _failatwith __FILE__ __LINE__ "unimp" *)

(* let () = *)
(*   Pp.printf "@{<bold>Close:@}\n"; *)
(*   Frontend.Typectx.pretty_print ctx'; *)
(*   Pp.printf "@{<bold>-@}\n"; *)
(*   Frontend.Typectx.pretty_print ctx; *)
(*   Pp.printf "@{<bold>=@}\n"; *)
(*   Frontend.Typectx.pretty_print *)
(*     { *)
(*       qvs = []; *)
(*       qbody = *)
(*         List.map (fun (b, (name, t)) -> (spf "|%b|%s" b name, t)) *)
(*         @@ Languages.UnderTypectx.subtract ctx'.qbody ctx.qbody; *)
(*     } *)
(* in *)
(* module MultiTypectx = Languages.MultiUnderTypectx *)
module Nctx = Languages.UTSimpleTypectx
module Typectx = Languages.MustMayTypectx
(* open Abstraction *)

type rec_info = {
  fix_name : string;
  rank_lhs : string;
  rank_rhs : Autov.Prop.lit;
}

type uctx = {
  rec_info : rec_info option;
  ctx : Typectx.ctx;
  nctx : Typectx.ctx;
  libctx : Nctx.ctx;
}

let full_projection_check uctx fty =
  match uctx.rec_info with
  | None -> _failatwith __FILE__ __LINE__ ""
  | Some { rank_lhs; _ } -> (
      let right_ty = UT.map_by_ghost_name fty (rank_lhs, fun _ -> None) in
      let left_ty = UT.reduce_inv_type_by_name fty rank_lhs in
      let () = Pp.printf "Full Projection Check:\n" in
      try
        let () = Inv_check.check (left_ty, right_ty) in
        Pp.printf "@{<bold>@{<yellow>Full Projection Check successed@}@}\n"
      with Autov.FailWithModel (msg, e) ->
        Pp.printf "@{<bold>@{<red>Full Projection Check failed@}@}\n";
        raise (Autov.FailWithModel (msg, e)))

let if_rec_call uctx fname argname =
  match (uctx.rec_info, fname) with
  | None, _ -> false
  | _, None -> false
  | Some { fix_name; rank_lhs; _ }, Some fname ->
      let () = Pp.printf "@{<bold>if_rec_call %s  %s@}\n" fix_name fname in
      String.equal fname fix_name && String.equal argname rank_lhs

let right_ty_measure_0 uctx ty =
  match uctx.rec_info with
  | None -> _failatwith __FILE__ __LINE__ ""
  | Some { rank_lhs; _ } ->
      UT.map_by_ghost_name ty
        ( rank_lhs,
          fun { basename; normalty; prop } ->
            let prop =
              P.(
                let x = Ntyped.{ x = basename; ty = normalty } in
                let prop' = MethodPred ("==", [ ACint 0; AVar x ]) in
                And [ prop; prop' ])
            in
            Some { basename; normalty; prop } )

let right_ty_measure_ind uctx ty =
  match uctx.rec_info with
  | None -> _failatwith __FILE__ __LINE__ ""
  | Some { rank_lhs; _ } ->
      UT.map_by_ghost_name ty
        ( rank_lhs,
          fun { basename; normalty; prop } ->
            let prop =
              P.(
                let x = Ntyped.{ x = basename; ty = normalty } in
                And [ prop; MethodPred ("<", [ ACint 0; AVar x ]) ])
            in
            Some { basename; normalty; prop } )

let left_ty_measure_0 uctx ty =
  match uctx.rec_info with
  | None -> _failatwith __FILE__ __LINE__ ""
  | Some { rank_lhs; _ } ->
      UT.map_by_ghost_name ty
        ( rank_lhs,
          fun { basename; normalty; _ } ->
            Some { basename; normalty; prop = P.mk_false } )

let left_ty_measure_i uctx ty =
  match uctx.rec_info with
  | None -> _failatwith __FILE__ __LINE__ ""
  | Some { rank_lhs; _ } ->
      UT.map_by_ghost_name ty
        ( rank_lhs,
          fun { basename; normalty; prop } ->
            Some
              {
                basename;
                normalty;
                prop =
                  P.(
                    let x = Ntyped.{ x = basename; ty = normalty } in
                    And
                      [
                        prop;
                        MethodPred
                          ( "<",
                            [
                              AVar x;
                              AVar Ntyped.{ x = rank_lhs; ty = normalty };
                            ] );
                      ]);
              } )

(* let derive_base_post_ty uctx ty = *)
(*   match uctx.rec_info with *)
(*   | None -> _failatwith __FILE__ __LINE__ "" *)
(*   | Some { rank_rhs; _ } -> *)
(*       let ind_prop = P.(MethodPred ("==", [ rank_rhs; ACint 0 ])) in *)
(*       let ty = UT.map_on_retty (fun p -> P.(peval (And [ p; ind_prop ]))) ty in *)
(*       Param.type_infer uctx.param_ctx ty *)

(* let derive_ind_pre_ty uctx ty = *)
(*   match uctx.rec_info with *)
(*   | None -> _failatwith __FILE__ __LINE__ "" *)
(*   | Some { rank_lhs; rank_rhs; _ } -> *)
(*       let ind_prop = *)
(*         P.(MethodPred ("<", [ rank_rhs; AVar { x = rank_lhs; ty = Ty_int } ])) *)
(*       in *)
(*       let ty = UT.map_on_retty (fun p -> P.(peval (And [ p; ind_prop ]))) ty in *)
(*       Param.type_infer uctx.param_ctx ty *)

(* let derive_ind_post_ty uctx ty = *)
(*   match uctx.rec_info with *)
(*   | None -> _failatwith __FILE__ __LINE__ "" *)
(*   | Some { rank_lhs; rank_rhs; _ } -> *)
(*       let ind_prop = *)
(*         P.(MethodPred ("==", [ rank_rhs; AVar { x = rank_lhs; ty = Ty_int } ])) *)
(*       in *)
(*       let ty = UT.map_on_retty (fun p -> P.(peval (And [ p; ind_prop ]))) ty in *)
(*       Param.type_infer uctx.param_ctx ty *)

let candidate_vars_by_nt uctx nt =
  let cs = Typectx.get_by_nt uctx.ctx nt in
  List.filter_map
    (fun (x, ty) ->
      match ty with
      | MMT.Ut ty ->
          let b = Reachability_check.reachability_check uctx ty in
          (* let () = *)
          (*   Pp.printf "candidate_vars_by_nt: %s <%s>\n" x.Ntyped.x *)
          (*     (UT.pretty_layout ty) *)
          (* in *)
          if b then Some x else None
      | MMT.Ot _ ->
          (* TODO: check ot *)
          Some x)
    cs

let term_subtyping_check file line uctx UL.{ x; ty } t2 =
  let () = Undersub.subtyping_check file line uctx.ctx ty t2 in
  UL.{ x; ty = t2 }

let term_subtyping_check_opt file line uctx UL.{ x; ty } t2 =
  if Undersub.subtyping_check_bool file line uctx.ctx ty t2 then
    Some UL.{ x; ty = t2 }
  else None

open UT

let id_type_infer_raw (uctx : uctx) (id : NL.id NL.typed) : MMT.t =
  try Typectx.get_ty uctx.ctx id.x with _ -> Ut (Nctx.get_ty uctx.libctx id.x)

let id_type_infer (uctx : uctx) (id : NL.id NL.typed) : UL.id UL.typed =
  let ty =
    try
      match Typectx.get_ty uctx.ctx id.x with
      | Ot _ ->
          (* let () = *)
          (*   Pp.printf "@{<yellow>infer id: %s => %s@}\n" id.x *)
          (*     (UT.ot_pretty_layout ot) *)
          (* in *)
          make_basic_from_eq_var { x = id.x; ty = snd id.ty }
      | Ut ut -> ut
    with _ -> Nctx.get_ty uctx.libctx id.x
  in
  erase_check_mk_id __FILE__ __LINE__ id ty

let id_type_check (uctx : uctx) (id : NL.id NL.typed) (ty : UT.t) :
    NL.id UL.typed =
  let id = id_type_infer uctx id in
  term_subtyping_check __FILE__ __LINE__ uctx id ty
(* let () = subtyping_check __FILE__ __LINE__ uctx id.UL.ty ty in *)
(* UL.{ x = id.x; ty } *)

let lit_type_infer (uctx : uctx) (lit : NL.smt_lit NL.typed) :
    UL.smt_lit UL.typed =
  let open NL in
  let open UT in
  match lit.x with
  | ConstI n -> { ty = make_basic_from_const_int n; x = ConstI n }
  | ConstB b -> { ty = make_basic_from_const_bool b; x = ConstB b }
  | Var id ->
      UL.(typed_map (fun x -> Var x))
      @@ id_type_infer uctx { ty = lit.ty; x = id }
