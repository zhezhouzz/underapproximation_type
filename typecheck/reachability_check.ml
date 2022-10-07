open Languages
module P = Autov.Prop

(* TODO: a real reachability check *)
let reachability_check _ ty =
  let res =
    match UT.assume_base_destruct_opt ty with
    | Some (_, _, prop) -> (
        P.(match peval prop with Lit (ACbool false) -> false | _ -> true))
    | _ -> true
  in
  (* let () = *)
  (*   Pp.printf "@{<bold>reachability_check@} %s: %b\n" (UT.pretty_layout ty) res *)
  (* in *)
  res
