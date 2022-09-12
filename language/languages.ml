include Ast

module Lemma = struct
  include Frontend.Lemma
  include Lemma

  (* open Sugar *)
  open Ntyped

  let ulemmas_with_dts ulemmas dts =
    List.filter_map
      (fun x ->
        match instantiate_dt x dts with
        | None -> None
        | Some (qvs, prop) -> Some (P.topu_to_prop (qvs, prop)))
      ulemmas

  let add_lemmas lemmas
      { vc_u_basics; vc_u_dts; vc_e_basics; vc_head; vc_e_dts; vc_body } =
    let ulemmas, elemmas = split_to_u_e lemmas in
    let vcl_body =
      let ulemmas = ulemmas_with_dts ulemmas vc_e_dts in
      P.And (ulemmas @ [ vc_body ])
    in
    let vcl_lemmas = List.map to_prop ulemmas in
    let elemmas =
      List.filter_map (fun x -> instantiate_dt x vc_u_dts) elemmas
    in
    (* let () = *)
    (*   Printf.printf "vc_head : %s\n" @@ Autov.pretty_layout_prop vc_head *)
    (* in *)
    let vcl_u_basics, vcl_head =
      match elemmas with
      | [] -> (vc_u_basics, vc_head)
      | _ -> (
          let u_basics', prop' = u_union elemmas in
          let () =
            Printf.printf
              "\t len(vc_u_dts)=%i len(elemmas)=%i len(prop') = %i\n"
              (List.length vc_u_dts) (List.length elemmas) (List.length prop')
          in
          match prop' with
          | [] -> (vc_u_basics, vc_head)
          | prop' -> (vc_u_basics @ u_basics', P.And (prop' @ [ vc_head ])))
    in
    (* let () = *)
    (*   Printf.printf "vcl_head : %s\n" @@ Autov.pretty_layout_prop vcl_head *)
    (* in *)
    let res =
      {
        vcl_lemmas;
        vcl_u_basics;
        vcl_u_dts = vc_u_dts;
        vcl_e_basics = vc_e_basics;
        vcl_head;
        vcl_e_dts = vc_e_dts;
        vcl_body;
      }
    in
    let () = pretty_print_with_lemma res in
    let vcl_u_basics', vcl_body =
      P.lift_qv_over_mp_in_uprop __FILE__ __LINE__ vcl_body vc_e_dts
    in
    (* let vcl_u_basics' = [] in *)
    let vcl_body =
      P.instantiate_uqvs_in_uprop __FILE__ __LINE__ vcl_body
        (vcl_u_basics @ vc_e_basics)
    in
    let res =
      {
        vcl_lemmas;
        vcl_u_basics = vcl_u_basics @ vcl_u_basics';
        vcl_u_dts = vc_u_dts;
        vcl_e_basics = vc_e_basics;
        vcl_head;
        vcl_e_dts = vc_e_dts;
        vcl_body;
      }
    in
    let () = pretty_print_with_lemma res in
    res

  let without_e_dt lemmas
      {
        vcl_lemmas;
        vcl_u_basics;
        vcl_u_dts;
        vcl_e_basics;
        vcl_head;
        vcl_e_dts;
        vcl_body;
      } =
    let _, _ = split_to_u_e lemmas in
    (* let () = *)
    (*   Printf.printf "vcl_body: %s\n" @@ Autov.pretty_layout_prop vcl_body *)
    (* in *)
    let flemmas = Abstraction.Prim_map.functional_lemmas_to_pres () in
    let flemmas = ulemmas_with_dts flemmas (vcl_u_dts @ vcl_e_dts) in
    let flemmas =
      P.instantiate_uqvs_in_uprop __FILE__ __LINE__ (And flemmas)
        (vcl_u_basics @ vcl_e_basics)
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
    without_e_dt lemmas x
end

module UnderTypectx = struct
  include Frontend.Utypectx
  include UnderTypectx
end

module UT = struct
  include Frontend.Underty
  include UT
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

  let layout x = Frontend.Expr.layout @@ Trans.nan_to_term x
  let typed_map f { ty; x } = { ty; x = f x }
end
