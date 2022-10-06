open Languages
module Typectx = UnderTypectx
module P = Autov.Prop
open Ntyped
open Zzdatatype.Datatype
(* open Abstraction *)

type infer_ctx = {
  qv : string typed list;
  args : string typed list;
  mps : string list;
  features : (string * string typed) list;
}

open Typecheck
open Infer_ctx
open Bv
open Sugar

let get_fvs infer_ctx model =
  let fv_tab =
    Autov.Func_interp.get_fvs infer_ctx.code_features
      (List.map (fun x -> x.x) (get_code_inpout infer_ctx))
      (List.map (fun x -> x.x) infer_ctx.qvs)
      model
  in
  let () = layout_bvs infer_ctx fv_tab in
  fv_tab

type chekck_result = Succ of UT.t | Failed of Z3.Model.model

let phi_init infer_ctx =
  let settings = List.map (fun _ -> [ true; false ]) infer_ctx.features in
  let all = List.choose_list_list settings in
  Hashtbl.of_seq @@ Seq.map (fun x -> (x, ())) @@ List.to_seq all

let phi_to_prop infer_ctx phi =
  match List.of_seq @@ Hashtbl.to_seq_keys phi with
  | [] -> P.mk_false
  | bvs ->
      let props = List.map (bv_to_prop infer_ctx.features) bvs in
      P.Or props

let neg_phi_to_prop infer_ctx phi =
  match phi with
  | [] -> P.mk_true
  | bvs ->
      let props =
        List.map (fun x -> P.Not (bv_to_prop infer_ctx.features x)) bvs
      in
      P.And props

let phi_rule_out_bv phi bv =
  if Hashtbl.mem phi bv then Hashtbl.remove phi bv
  else _failatwith __FILE__ __LINE__ ""

let conjunct_ty_with_phi infer_ctx uty neg_phi =
  UT.work_on_retty
    (fun x -> String.equal x infer_ctx.retv.x)
    ( (fun prop ->
        And
          [
            prop;
            P.topu_to_prop
              (infer_ctx.qvs, P.Not (bv_to_prop_merge infer_ctx neg_phi));
          ]),
      fun _ -> _failatwith __FILE__ __LINE__ "" )
    uty

let add_to_phi bv bvs =
  if List.exists (fun x -> List.equal ( == ) x bv) bvs then
    let () =
      Pp.printf "@{<bold>Err:@}: dup %s" (List.split_by_comma string_of_bool bv)
    in
    _failatwith __FILE__ __LINE__ ""
  else bv :: bvs

let lam_post_shrink infer_ctx uctx prog uty =
  let counter = ref 0 in
  let rec aux neg_phi =
    let () = counter := !counter + 1 in
    let () = Pp.printf "@{<bold>Iter: %i@}\n" !counter in
    (* let neg_phi_prop = neg_phi_to_prop infer_ctx neg_phi in *)
    (* let () = *)
    (*   Pp.printf "@{<yellow>neg_phi:@} %s\n" *)
    (*   @@ Autov.pretty_layout_prop (bv_to_prop_merge infer_ctx neg_phi) *)
    (* in *)
    (* let () = *)
    (*   Printf.printf "prop: %s\n" @@ Autov.pretty_layout_prop neg_phi_prop *)
    (* in *)
    (* let _ = *)
    (*   match *)
    (*     Autov.check [] *)
    (*       (P.Iff (neg_phi_prop, P.Not (bv_to_prop_merge infer_ctx neg_phi))) *)
    (*   with *)
    (*   | None -> () *)
    (*   | Some _ -> _failatwith __FILE__ __LINE__ "bad" *)
    (* in *)
    let uty' = conjunct_ty_with_phi infer_ctx uty neg_phi in
    (* let () = Printf.printf "uty: %s\n" @@ UT.pretty_layout uty' in *)
    let res =
      try
        let _ = Undercheck.type_check uctx prog uty' in
        Succ uty'
      with Autov.FailWithModel (_, model) -> Failed model
    in
    match res with
    | Succ uty -> uty
    | Failed model ->
        let bvs = get_fvs infer_ctx model in
        let bv = choose_bv bvs in
        let () = Pp.printf "@{<bold>Add:@} %s\n" (layout_bv bv) in
        aux (add_to_phi bv neg_phi)
  in
  aux []

let rec_post_shrink infer_ctx uctx (f, prog) uty =
  let inner_counter = ref 0 in
  let outer_counter = ref 0 in
  let rec aux neg_phi_in_ctx neg_phi =
    let () = inner_counter := !inner_counter + 1 in
    let () =
      Pp.printf "@{<bold>Rec Iter: %i -- %i@}\n" !outer_counter !inner_counter
    in
    let uty_in_ctx = conjunct_ty_with_phi infer_ctx uty neg_phi_in_ctx in
    let uty' = conjunct_ty_with_phi infer_ctx uty neg_phi in
    (* let () = Printf.printf "uty_in_ctx: %s\n" @@ UT.pretty_layout uty_in_ctx in *)
    (* let () = Printf.printf "uty: %s\n" @@ UT.pretty_layout uty' in *)
    let res =
      try
        let _ =
          Undercheck.(
            term_type_check
              {
                uctx with
                libctx =
                  Typectx.force_add_to_right uctx.libctx
                    UL.{ x = f.NL.x; ty = uty_in_ctx };
              }
              prog uty')
        in
        Succ uty'
      with Autov.FailWithModel (_, model) -> Failed model
    in
    match res with
    | Succ _ -> neg_phi
    | Failed model ->
        let bvs = get_fvs infer_ctx model in
        let bv = choose_bv bvs in
        let () = Pp.printf "@{<bold>Add:@} %s\n" (layout_bv bv) in
        aux neg_phi_in_ctx (add_to_phi bv neg_phi)
  in
  let rec loop neg_phi_in_ctx =
    let () = outer_counter := !outer_counter + 1 in
    let neg_phi = aux neg_phi_in_ctx neg_phi_in_ctx in
    if List.length neg_phi == List.length neg_phi_in_ctx then
      conjunct_ty_with_phi infer_ctx uty neg_phi
    else loop neg_phi
  in
  loop []

let rec_post_shrink_v2 infer_ctx uctx (f, prog) uty =
  let counter = ref 0 in
  let rec aux neg_phi =
    let () = counter := !counter + 1 in
    let () = Pp.printf "@{<bold>Rec Iter: %i@}\n" !counter in
    let uty' = conjunct_ty_with_phi infer_ctx uty neg_phi in
    let res =
      try
        let _ =
          Undercheck.(
            term_type_check
              {
                uctx with
                libctx =
                  Typectx.force_add_to_right uctx.libctx
                    UL.{ x = f.NL.x; ty = uty' };
              }
              prog uty')
        in
        Succ uty'
      with Autov.FailWithModel (_, model) -> Failed model
    in
    match res with
    | Succ _ -> uty'
    | Failed model ->
        let bvs = get_fvs infer_ctx model in
        let bv = choose_bv bvs in
        let () =
          Pp.printf "@{<bold>Add:@} %s\n"
          @@ Autov.pretty_layout_prop
          @@ bv_to_prop infer_ctx.features bv
        in
        aux (add_to_phi bv neg_phi)
  in
  aux []

let post_shrink infer_ctx uctx prog uty =
  let open NL in
  match prog.x with
  | V (Lam _) -> lam_post_shrink infer_ctx uctx prog uty
  | V (Fix (f, prog)) ->
      (* rec_post_shrink infer_ctx nctx (f, { x = V prog.x; ty = prog.ty }) uty *)
      rec_post_shrink_v2 infer_ctx uctx (f, { x = V prog.x; ty = prog.ty }) uty
  | _ -> _failatwith __FILE__ __LINE__ ""

module SNA = Languages.StrucNA
module Nctx = Simpletypectx.UTSimpleTypectx

let get_inpout prog uty =
  let open UT in
  let rec aux prog uty =
    match (prog, uty) with
    | NL.(V (Fix (_, body))), _ -> aux NL.(V body.x) uty
    | _, UnderTy_base { basename; normalty; _ } ->
        ([], { x = basename; ty = normalty })
    | NL.(V (Lam (arg, _, body))), UnderTy_arrow { argname; argty; retty } ->
        let args, ret = aux body.NL.x retty in
        if is_base_type argty then
          let _, ty, _ = assume_base __FILE__ __LINE__ argty in
          (({ x = arg.NL.x; ty }, { x = argname; ty }) :: args, ret)
        else (args, ret)
    | _ -> _failatwith __FILE__ __LINE__ ""
  in
  aux prog.NL.x uty

open Checkaux

let struc_post_shrink infer_ctx_file l notations libs r =
  let open SNA in
  List.fold_lefti
    (fun tab id (info, ((name' : string), ty)) ->
      let id = id + 1 in
      let () = Pp.printf "@{<bold>Task %i:@}\n" id in
      match List.find_opt (fun { name; _ } -> String.equal name name') l with
      | None -> _failatwith __FILE__ __LINE__ "does not provide source code"
      | Some { body; _ } ->
          let () =
            match info with
            | None -> Pp.printf "@{<bold>No Inv:@}\n"
            | Some (id, prop) ->
                Pp.printf "@{<bold>Inv:@} %s = %s\n" id
                  (Autov.pretty_layout_lit prop)
          in
          let nctx =
            Nctx.(
              List.fold_left
                (fun ctx (name, ty) -> add_to_right ctx (name, ty))
                empty notations)
          in
          let libctx =
            List.fold_left
              (fun ctx (x, ty) -> Typectx.force_add_to_right ctx { x; ty })
              Typectx.empty libs
          in
          let ctx = Typectx.empty in
          let args, retv = get_inpout body ty in
          let infer_ctx = load infer_ctx_file args retv in
          let param_ctx = Param.empty in
          let rec_info =
            match (info, body.x) with
            | Some (rank_lhs, rank_rhs), NL.(V (Fix (f, _))) ->
                Some { fix_name = f.x; rank_lhs; rank_rhs }
            | None, NL.(V (Fix (fix_name, _))) ->
                failwith
                  (spf "No ranking function for rec function %s" fix_name.x)
            | Some (rank_lhs, _), _ ->
                failwith (spf "Useless ranking function %s" rank_lhs)
            | None, _ -> None
          in
          let uty =
            post_shrink infer_ctx
              { param_ctx; nctx; ctx; libctx; rec_info }
              body ty
          in
          tab @ [ (id, name', uty, Check_false.check infer_ctx uty) ])
    [] r
