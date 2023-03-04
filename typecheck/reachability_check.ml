open Languages
module P = Autov.Prop

(* TODO: a real reachability check *)

module Typectx = MustMayTypectx

let do_reach_check ctx t =
  let rec aux ctx t =
    match Typectx.destrct_right ctx with
    | None -> t
    | Some (ctx, (id, MMT.Ot UT.{ basename; normalty; prop })) ->
        let t =
          UT.retty_add_ex_uprop_drop_independent
            (id, UT.(UnderTy_base { basename; normalty; prop }))
            t
        in
        aux ctx t
    | Some (ctx, (id, MMT.Consumed uty)) | Some (ctx, (id, MMT.Ut uty)) ->
        let t' =
          if UT.is_base_type uty then
            UT.retty_add_ex_uprop_always_add (id, uty) t
          else t
        in
        (* let () = *)
        (*   Pp.printf "%s: %s ~ %s ==> %s\n" id (UT.pretty_layout uty) *)
        (*     (UT.pretty_layout t) (UT.pretty_layout t') *)
        (* in *)
        aux ctx t'
    | Some (ctx, (_, MMT.NoRefinement _)) -> aux ctx t
  in
  let t = aux ctx t in
  let name, nt, prop = UT.assume_base __FILE__ __LINE__ t in
  let eqs, prop = P.assume_tope_uprop __FILE__ __LINE__ prop in
  let nu = Ntyped.{ x = name; ty = nt } in
  let final_uqvs = [] in
  let final_eqvs = nu :: eqs in
  let final_pre = P.mk_true in
  let final_post = prop in
  let () =
    Typectx.pretty_print_q
      (List.map (fun x -> x.Ntyped.x) final_uqvs)
      (List.map (fun x -> x.Ntyped.x) final_eqvs)
      final_pre final_post
  in
  try
    let _ =
      Undersub.do_check __FILE__ __LINE__
        (final_uqvs, final_eqvs, final_pre, final_post)
    in
    true
  with
  | Autov.FailWithModel (_, _) -> false
  | Autov.SMTTIMEOUT ->
      let () =
        Pp.printf "@{<orange>Reachability_check failed:@}%s\n" "timeout"
      in
      false
  | e -> raise e

let reachability_check ctx ty =
  let () =
    Pp.printf "@{<bold>reachability_check@}\n";
    Typectx.pretty_print ctx;
    Pp.printf "|> %s\n" (UT.pretty_layout ty)
  in
  let res =
    match UT.assume_base_destruct_opt ty with
    | Some _ -> do_reach_check ctx ty
    | _ -> true
  in
  let () =
    Pp.printf "@{<bold>reachability_check@} %s: %b\n" (UT.pretty_layout ty) res
  in
  (* let () = failwith "end" in *)
  res
