open Languages
open Sugar

let check_empty_hidden file line hvs =
  if List.length hvs != 0 then
    _failatwith file line "do not allow hidden variables"
  else ()

let erase_check file line (underfty, normalty) =
  let _ = _check_equality file line NT.eq (UT.erase underfty) (snd normalty) in
  ()

let erase_check_mk_id file line id underfty =
  (* let () = *)
  (*   Pp.printf "|_ %s _| ~> %s = %s\n" *)
  (*     (Frontend.Underty.layout underfty) *)
  (*     (Frontend.Type.layout @@ UT.erase underfty) *)
  (*     (Frontend.Type.layout id.NL.ty) *)
  (* in *)
  let _ = _check_equality file line NT.eq (UT.erase underfty) (snd id.NL.ty) in
  UL.{ ty = underfty; x = id.x }

let subtyping_check = Undersub.subtyping_check

let term_subtyping_check file line ctx UL.{ x; ty } t2 =
  let () = Undersub.subtyping_check file line ctx ty t2 in
  UL.{ x; ty = t2 }

(* let subtyping_check__with_hidden_vars = *)
(*   Undersub.subtyping_check_with_hidden_vars *)

let merge_case_tys tys =
  let () =
    List.iteri
      (fun i ty ->
        Pp.printf "@{<bold>Case(%i) ty@}: %s\n" i @@ UT.pretty_layout ty)
      tys
  in
  (* let () = *)
  (*   Pp.printf "@{<bold>Compare@}\n"; *)
  (*   Frontend.Typectx.pretty_print ctx; *)
  (*   Frontend.Typectx.pretty_print true_branch_ctx *)
  (* in *)
  let ty = UT.disjunct_list tys in
  let () =
    Pp.printf "@{<bold>Merged ty@}: %s\n" @@ Frontend.Underty.pretty_layout ty
  in
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
