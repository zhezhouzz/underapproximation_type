include Ast

module Lemma = struct
  include Frontend.Lemma
  include Lemma

  (* open Sugar *)
  open Ntyped

  let lemmas_with_dts lemmas dts =
    List.map (fun x -> instantiate_dt x dts) lemmas

  let body_lift_emp res =
    let vcl_u_basics', vcl_body =
      P.lift_qv_over_mp_in_uprop __FILE__ __LINE__ res.vcl_body res.vcl_e_dts
    in
    {
      res with
      vcl_u_basics = res.vcl_u_basics @ vcl_u_basics';
      vcl_body = P.peval vcl_body;
    }

  let body_lift_all res =
    let vcl_u_basics', vcl_body =
      P.lift_merge_uprop __FILE__ __LINE__ res.vcl_body
    in
    {
      res with
      vcl_u_basics = res.vcl_u_basics @ vcl_u_basics';
      vcl_body = P.peval vcl_body;
    }

  let get_basics =
    List.filter (fun x -> match x.ty with Ty_int -> true | _ -> false)

  open Zzdatatype.Datatype

  let body_instantiate_uqvs res =
    let basics1 = get_basics (res.vcl_u_basics @ res.vcl_e_basics) in
    let vcl_body1 =
      P.instantiate_uqvs_in_uprop __FILE__ __LINE__ res.vcl_body basics1
    in
    let () =
      Pp.printf
        "@{<bold>body_instantiate_uqvs:@} basics1(%i)(%s) --> vc_body(%i)\n"
        (List.length basics1)
        (List.split_by_comma (fun x -> x.x) basics1)
        (P.size vcl_body1)
    in
    (* let basics2 = get_basics @@ P.tvar_fv res.vcl_body in *)
    (* let vcl_body2 = *)
    (*   P.instantiate_uqvs_in_uprop __FILE__ __LINE__ res.vcl_body basics2 *)
    (* in *)
    (* let () = *)
    (*   Pp.printf *)
    (*     "@{<bold>body_instantiate_uqvs:@} basics2(%i)(%s) --> vc_body(%i)\n" *)
    (*     (List.length basics2) *)
    (*     (List.split_by_comma (fun x -> x.x) basics2) *)
    (*     (P.size vcl_body2) *)
    (* in *)
    { res with vcl_body = vcl_body1 }

  let add_lemmas lemmas
      { vc_u_basics; vc_u_dts; vc_e_basics; vc_head; vc_e_dts; vc_body } =
    let ulemmas, elemmas = split_to_u_e lemmas in
    let vc_e_basics', vcl_body =
      let ulemmas = lemmas_with_dts ulemmas vc_e_dts in
      let elemmas = lemmas_with_dts elemmas vc_e_dts in
      let eqvs, elemmas =
        List.split
        @@ List.map
             (fun e -> P.rename_destruct_eprop __FILE__ __LINE__ e)
             elemmas
      in
      (List.concat eqvs, P.And (elemmas @ ulemmas @ [ vc_body ]))
    in
    let vcl_lemmas = List.map to_prop ulemmas in
    let elemmas = lemmas_with_dts lemmas vc_u_dts in
    let vcl_head =
      P.peval @@ P.conjunct_tope_uprop __FILE__ __LINE__ (elemmas @ [ vc_head ])
    in
    let vcl_u_basics, vcl_head =
      P.assume_tope_uprop __FILE__ __LINE__ vcl_head
    in
    let uqvs_head, vcl_head = P.lift_uprop __FILE__ __LINE__ vcl_head in
    let res =
      {
        vcl_lemmas;
        vcl_u_basics = vc_u_basics @ vcl_u_basics;
        vcl_u_dts = vc_u_dts;
        vcl_e_basics = vc_e_basics @ vc_e_basics' @ uqvs_head;
        vcl_head = P.peval vcl_head;
        vcl_e_dts = vc_e_dts;
        vcl_body = P.peval vcl_body;
      }
    in
    (* let () = *)
    (*   Pp.printf "@{<bold>raw:@} vc_head(%i); vc_body(%i)\n" *)
    (*     (P.size res.vcl_head) (P.size res.vcl_body) *)
    (* in *)
    (* let () = pretty_print_with_lemma res in *)
    let res = body_instantiate_uqvs res in
    (* let () = pretty_print_with_lemma res in *)
    res

  let without_e_dt
      {
        vcl_lemmas;
        vcl_u_basics;
        vcl_u_dts;
        vcl_e_basics;
        vcl_head;
        vcl_e_dts;
        vcl_body;
      } =
    (* let () = *)
    (*   Printf.printf "vcl_body: %s\n" @@ Autov.pretty_layout_prop vcl_body *)
    (* in *)
    let flemmas = Abstraction.Prim_map.functional_lemmas_to_pres () in
    let flemmas = lemmas_with_dts flemmas (vcl_u_dts @ vcl_e_dts) in
    (* let () = *)
    (*   List.iter *)
    (*     (fun x -> Printf.printf "flemmas: %s\n" @@ Autov.pretty_layout_prop x) *)
    (*     flemmas *)
    (* in *)
    let basics = get_basics @@ P.tvar_fv vcl_body in
    let flemmas =
      P.instantiate_uqvs_in_uprop_no_eq __FILE__ __LINE__ (And flemmas) basics
    in
    let vcl_body = P.And [ flemmas; vcl_body ] in
    let vclw_e_basics', vclw_body =
      Autov.uqv_encoding (List.map (fun x -> x.x) vcl_e_dts) vcl_body
    in
    let res =
      {
        vclw_lemmas = vcl_lemmas;
        vclw_u_basics = vcl_u_basics;
        vclw_u_dts = vcl_u_dts;
        vclw_e_basics = vcl_e_basics @ vclw_e_basics';
        vclw_body = P.Implies (vcl_head, vclw_body);
      }
    in
    res

  let query_with_lemma_to_prop
      { vclw_lemmas; vclw_u_basics; vclw_u_dts; vclw_e_basics; vclw_body } =
    let if_snf = true in
    if if_snf then
      ( vclw_lemmas,
        vclw_u_basics @ vclw_u_dts,
        List.fold_right
          (fun x prop -> P.Exists (x, prop))
          vclw_e_basics vclw_body )
    else
      ( vclw_lemmas,
        [],
        List.fold_right
          (fun x prop -> P.Forall (x, prop))
          (vclw_u_basics @ vclw_u_dts)
        @@ List.fold_right
             (fun x prop -> P.Exists (x, prop))
             vclw_e_basics vclw_body )

  let with_lemma lemmas (uqvs, eqvs, vc_head, vc_body) =
    let () =
      Pp.printf "@{<bold>raw:@} vc_head(%i); vc_body(%i)\n" (P.size vc_head)
        (P.size vc_body)
    in
    let mps = P.get_mps (Implies (vc_head, vc_body)) in
    let lemmas =
      List.filter (fun x -> List.exists (fun y -> eq y.ty x.udt.ty) mps) lemmas
    in
    let vc_u_dts, vc_u_basics = List.partition (fun x -> is_dt x.ty) uqvs in
    let vc_e_dts, vc_e_basics = List.partition (fun x -> is_dt x.ty) eqvs in
    let x =
      add_lemmas lemmas
        { vc_u_basics; vc_u_dts; vc_e_basics; vc_head; vc_e_dts; vc_body }
    in
    let () =
      Pp.printf "@{<bold>add_lemma:@} vc_head(%i); vc_body(%i)\n"
        (P.size x.vcl_head) (P.size x.vcl_body)
    in
    let x = without_e_dt x in
    let () = Pp.printf "@{<bold>without_dt:@} %i\n" (P.size x.vclw_body) in
    (* let () = failwith "zz" in *)
    x
end

module UT = struct
  include Frontend.Underty
  include UT
end

module MustMayTypectx = struct
  include Frontend.Typectx
  include MustMayTypectx
  open Sugar
  open MMT
  open Ntyped

  let subtract ctx ctx' =
    let rec aux = function
      | ctx, [] -> ctx
      | [], _ -> _failatwith __FILE__ __LINE__ ""
      | (name, ty) :: ctx, (name', ty') :: ctx' ->
          if String.equal name name' && MMT.eq ty ty' then aux (ctx, ctx')
          else _failatwith __FILE__ __LINE__ ""
    in
    aux (ctx, ctx')

  let destrct_right ctx =
    match List.rev ctx with [] -> None | h :: t -> Some (List.rev t, h)

  let get_by_nt (ctx : ctx) nt =
    List.filter_map
      (fun (name, ty) ->
        if eq nt (erase ty) then Some { x = name; ty = nt } else None)
      ctx

  let ut_update reachability_check ctx (name, f) =
    let rec aux res = function
      | [] -> _failatwith __FILE__ __LINE__ ""
      | (name', ty) :: rest ->
          if String.equal name name' then
            match ty with
            | Ut ty ->
                let ty' = f ty in
                if reachability_check ctx ty' then
                  Some (res @ ((name', Ut ty') :: rest))
                else None
            | Ot _ -> _failatwith __FILE__ __LINE__ ""
          else aux (res @ [ (name', ty) ]) rest
    in
    aux [] ctx

  let ot_update reachability_check ctx (name, f) =
    let open UT in
    let rec aux res = function
      | [] -> _failatwith __FILE__ __LINE__ ""
      | (name', ty) :: rest ->
          if String.equal name name' then
            match ty with
            | Ut _ -> _failatwith __FILE__ __LINE__ ""
            | Ot ot ->
                let { basename; normalty; prop } = f ot in
                if
                  reachability_check ctx
                    (UT.UnderTy_base { basename; normalty; prop })
                then
                  Some (res @ ((name', Ot { basename; normalty; prop }) :: rest))
                else None
          else aux (res @ [ (name', ty) ]) rest
    in
    aux [] ctx

  let consume ctx name =
    let rec aux res = function
      | [] -> _failatwith __FILE__ __LINE__ ""
      | (name', ty) :: rest ->
          if String.equal name name' then
            match ty with
            | Ut ty ->
                res @ ((name', Ut (UT.make_basic_bot (UT.erase ty))) :: rest)
            | Ot _ -> _failatwith __FILE__ __LINE__ ""
          else aux (res @ [ (name', ty) ]) rest
    in
    aux [] ctx

  let ut_force_add_to_right ctx id = add_to_right ctx (id.UL.x, Ut id.ty)

  let ut_force_add_to_rights ctx ids =
    List.fold_left (fun ctx id -> ut_force_add_to_right ctx id) ctx ids

  let ut_add_to_right reachability_check ctx id =
    if reachability_check ctx id.UL.ty then Some (ut_force_add_to_right ctx id)
    else None

  let ut_add_to_rights reachability_check ctx ids =
    List.fold_left
      (fun ctx id ->
        match ctx with
        | None -> None
        | Some ctx -> ut_add_to_right reachability_check ctx id)
      (Some ctx) ids

  let ot_add_to_right ctx (id, ot) = add_to_right ctx (id, Ot ot)

  let ot_add_to_rights ctx ids =
    List.fold_left (fun ctx id -> ot_add_to_right ctx id) ctx ids

  (* Assume everything in the type context is not bot *)
  let close_by_diff_ diff uty = List.fold_right UT.retty_add_ex_uprop diff uty

  let close_by_diff ctx ctx' uty =
    let diff = subtract ctx ctx' in
    let diff =
      List.map
        (function
          | x, Ut ut -> (x, ut) | _, Ot _ -> _failatwith __FILE__ __LINE__ "")
        diff
    in
    close_by_diff_ diff uty

  let check_in x p = List.exists (String.equal x) @@ Autov.prop_fv p

  (* let _assume_basety file line (x, ty) = *)
  (*   match ty with *)
  (*   | UnderTy_base { basename; prop; normalty } -> *)
  (*       let prop = P.subst_id prop basename x in *)
  (*       ({ ty = normalty; x }, prop) *)
  (*   | _ -> *)
  (*       let () = Printf.printf " %s: %s\n" x (pretty_layout ty) in *)
  (*       _failatwith file line "should not happen" *)

  (* let close_prop_ if_drop_unused ctx prop = *)
  (*   let open P in *)
  (*   let rec aux ctx prop = *)
  (*     match destrct_right ctx with *)
  (*     | None -> prop *)
  (*     | Some (ctx, (x, xty)) -> *)
  (*         (\* let xty = conjunct_list xty in *\) *)
  (*         (\* NOTE: the lambda type always indicates values, thus are reachable *\) *)
  (*         if is_base_type xty then *)
  (*           let x, xprop = _assume_basety __FILE__ __LINE__ (x, xty) in *)
  (*           if if_drop_unused && not (check_in x.x prop) then aux ctx prop *)
  (*           else *)
  (*             let xeqvs, xprop = P.assume_tope_uprop __FILE__ __LINE__ xprop in *)
  (*             let eqvs, prop = P.assume_tope_uprop __FILE__ __LINE__ prop in *)
  (*             let prop' = *)
  (*               P.conjunct_eprop_to_right_ (x :: xeqvs, xprop) (eqvs, prop) *)
  (*             in *)
  (*             (\* let _ = *\) *)
  (*             (\*   Pp.printf "@{<bold>Conj:@} %s --> %s = %s\n" *\) *)
  (*             (\*     (Autov.pretty_layout_prop xprop) *\) *)
  (*             (\*     (Autov.pretty_layout_prop prop) *\) *)
  (*             (\*     (Autov.pretty_layout_prop prop') *\) *)
  (*             (\* in *\) *)
  (*             aux ctx prop' *)
  (*         else aux ctx prop *)
  (*   in *)
  (*   let res = aux ctx prop in *)
  (*   let res' = peval res in *)
  (*   (\* let () = *\) *)
  (*   (\*   Pp.printf "@{<bold>PEVAL:@}\n %s\n=%s\n" *\) *)
  (*   (\*     (Autov.pretty_layout_prop res) *\) *)
  (*   (\*     (Autov.pretty_layout_prop res') *\) *)
  (*   (\* in *\) *)
  (*   res' *)

  (* let close_prop = close_prop_ false *)
  (* let close_prop_drop_independt = close_prop_ true *)

  (* let close_type uty nu = *)
  (*   let open P in *)
  (*   let rec aux uty = *)
  (*     match uty with *)
  (*     | UnderTy_base _ -> *)
  (*         let _, prop = _assume_basety __FILE__ __LINE__ (nu, uty) in *)
  (*         prop *)
  (*     | UnderTy_arrow { argname; argty; retty } -> *)
  (*         (\* let _ = *\) *)
  (*         (\*   Printf.printf "%s is base type: %b\n" (pretty_layout argty) *\) *)
  (*         (\*     (is_base_type argty) *\) *)
  (*         (\* in *\) *)
  (*         if is_base_type argty then *)
  (*           let x, xprop = _assume_basety __FILE__ __LINE__ (argname, argty) in *)
  (*           Exists (x, And [ xprop; aux retty ]) *)
  (*         else aux retty *)
  (*     | _ -> _failatwith __FILE__ __LINE__ "" *)
  (*   in *)
  (*   let res = aux uty in *)
  (*   let res' = peval res in *)
  (*   res' *)
end

module Typedec = struct
  include Frontend.Typedec
  include Typedec
end

module Struc = struct
  include Frontend.Structure
  include Struc

  let prog_of_ocamlstruct = Frontend.Structure.client_of_ocamlstruct
end

module NL = struct
  include NL

  let layout x = Frontend.Expr.layout @@ Trans.nan_to_term x
  let layout_value v = layout { x = V v.x; ty = v.ty }
  let layout_id x = layout_value { x = Lit (Var x.x); ty = x.ty }
end

module StrucNA = struct
  include StrucNA

  let prog_of_ocamlstruct = Frontend.Structure.client_of_ocamlstruct
  let layout code = Struc.layout @@ Trans.struc_nan_to_term code
end

module OT = struct
  include Frontend.Overty
  include OT
end

module UL = struct
  include UL

  (* let layout x = *)
  (*   let ty = UT.erase x.ty in *)
  (*   let x = x.x in *)
  (*   Frontend.Expr.layout @@ Trans.nan_to_term NL.{ x; ty = (None, ty) } *)

  (* let layout_value x = *)
  (*   let x = { x = V x.x; ty = x.ty } in *)
  (*   layout x *)

  let typed_map f { ty; x } = { ty; x = f x }

  let get_args_return_name retname body =
    let open Anormal.NormalAnormal in
    let rec aux body =
      match body.x with
      | V (Lam (x, _, body)) ->
          let args, retv = aux body in
          (NNtyped.to_ntyped x :: args, retv)
      | _ -> ([], NNtyped.to_ntyped { x = retname; ty = body.ty })
    in
    aux body
end
