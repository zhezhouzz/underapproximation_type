let lemmas_with_dts lemmas dts = List.map (fun x -> instantiate_dt x dts) lemmas

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
  (* let () = *)
  (*   Pp.printf *)
  (*     "@{<bold>body_instantiate_uqvs:@} basics1(%i)(%s) --> vc_body(%i)\n" *)
  (*     (List.length basics1) *)
  (*     (List.split_by_comma (fun x -> x.x) basics1) *)
  (*     (P.size vcl_body1) *)
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
      @@ List.map (fun e -> P.rename_destruct_eprop __FILE__ __LINE__ e) elemmas
    in
    (List.concat eqvs, P.And (elemmas @ ulemmas @ [ vc_body ]))
  in
  let vcl_lemmas = List.map to_prop ulemmas in
  let elemmas = lemmas_with_dts lemmas vc_u_dts in
  let vcl_head =
    P.peval @@ P.conjunct_tope_uprop __FILE__ __LINE__ (elemmas @ [ vc_head ])
  in
  let vcl_u_basics, vcl_head = P.assume_tope_uprop __FILE__ __LINE__ vcl_head in
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
      List.fold_right (fun x prop -> P.Exists (x, prop)) vclw_e_basics vclw_body
    )
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
    Env.show_debug_stat @@ fun _ ->
    Pp.printf "@{<bold>raw:@} vc_head(%i); vc_body(%i)\n" (P.size vc_head)
      (P.size vc_body)
  in
  let mps = P.get_mps (Implies (vc_head, vc_body)) in
  let lemmas =
    List.filter
      (fun x ->
        List.for_all
          (fun udt -> List.exists (fun y -> eq y.ty udt.ty) mps)
          x.lemma_udts)
      lemmas
  in
  let vc_u_dts, vc_u_basics = List.partition (fun x -> is_dt x.ty) uqvs in
  let vc_e_dts, vc_e_basics = List.partition (fun x -> is_dt x.ty) eqvs in
  let x =
    add_lemmas lemmas
      { vc_u_basics; vc_u_dts; vc_e_basics; vc_head; vc_e_dts; vc_body }
  in
  let () =
    Env.show_debug_stat @@ fun _ ->
    Pp.printf "@{<bold>add_lemma:@} vc_head(%i); vc_body(%i)\n"
      (P.size x.vcl_head) (P.size x.vcl_body)
  in
  (* let () = if P.size x.vcl_body > 130000 then failwith "timeout" else () in *)
  let x = without_e_dt x in
  let () =
    Env.show_debug_stat @@ fun _ ->
    Pp.printf "@{<bold>without_dt:@} %i\n" (P.size x.vclw_body)
  in
  x
