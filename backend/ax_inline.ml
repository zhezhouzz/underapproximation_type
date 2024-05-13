open Zzdatatype.Datatype
(** Inline the existential quantified axioms to aviod timeout.
    1. Make the query in \Forall\Exists quatified formula.
    2. Inline the axioms with resepct to the quantified variables, then record which variable is already inlined.
    3. Send to SMT solver, if solved, then down; otherwise go to 1. (Set a unrolling bound)

    query: forall (v: int list). p
    axiom: forall (l: int list). (not (emp l)) #==> (exists h t. hd l h && tl l t).
    Inline (iteration 1):
    1. forall (v: int list). p
    2. forall (v: int list). ((not (emp v)) #==> (exists h t. hd v h && tl v t)) #==> p #### recode that v is inlined.
    Inline (iteration 2):
    1. forall (v: int list). exists (h: int). exists (t: int list). ((not (emp v)) #==> (hd v h && tl v t)) #==> p
    2. forall (v: int list). exists (h: int). exists (t: int list).
      #### we will not inline v again.
      ((not (emp t)) #==> (exists h' t'. hd t h' && tl t t')) #==>
      (((not (emp v)) #==> (hd v h && tl v t)) #==> p)

    Note that, we are always in EPR (\Forall\Exists quatified).
*)

open Sugar
open Language

type inline_setting = (Nt.t, string) typed list

let layout_setting l = spf "(%s)" @@ List.split_by_comma (fun x -> x.x) l

type inline_ctx = {
  ax_names : string list;
  axs : (string, Nt.t prop) Hashtbl.t;
  inlined : (string, inline_setting list) Hashtbl.t;
}

let inline_ctx_init axioms =
  let axioms, fa_axioms =
    List.partition
      (fun (_, prop) ->
        let exqvs, _ = gather_ex_qvs true @@ snd (gather_fa_qvs false prop) in
        List.length exqvs > 0)
      axioms
  in
  let ax_names = List.map fst axioms in
  let axs = Hashtbl.create (List.length ax_names) in
  let inlined = Hashtbl.create (List.length ax_names) in
  let () = List.iter (fun (x, prop) -> Hashtbl.add axs x prop) axioms in
  let () = List.iter (fun x -> Hashtbl.add inlined x []) ax_names in
  ({ ax_names; axs; inlined }, fa_axioms)

let setting_eq s1 s2 = List.eq (fun x y -> String.equal x.x y.x) s1 s2

let update_inlined ctx (name, settings) =
  let settings' = Hashtbl.find ctx.inlined name in
  Hashtbl.replace ctx.inlined name (settings' @ settings)

let filter_inlined ctx (name, settings) =
  let settings' = Hashtbl.find ctx.inlined name in
  List.filter (fun xs -> not (List.exists (setting_eq xs) settings')) settings

let find_inline_setting qvs prop =
  let faqvs, _ = gather_fa_qvs false prop in
  (* let () = Printf.printf "faqvs: %s\n" (layout_setting faqvs) in *)
  let tps = List.map _get_ty faqvs in
  let options =
    List.map (fun tp -> List.filter (fun x -> Nt.eq x.ty tp) qvs) tps
  in
  (* let () = *)
  (*   Printf.printf "options: %s\n" *)
  (*     (List.split_by " | " (fun x -> List.split_by_comma _get_x x) options) *)
  (* in *)
  let settings = List.choose_list_list options in
  settings

let alpha_renaming_ex_prop prop =
  let exqvs, prop = gather_ex_qvs true prop in
  let exqvs' = List.map (fun x -> (Rename.unique x.x) #: x.ty) exqvs in
  let mapping = _safe_combine __FILE__ __LINE__ exqvs exqvs' in
  let prop =
    List.fold_left
      (fun prop (x, y) -> subst_prop_instance x.x (AVar y) prop)
      prop mapping
  in
  let prop = List.fold_right (fun qv body -> Exists { qv; body }) exqvs' prop in
  prop

let specialize_fa_qvs setting prop =
  let faqvs, prop = gather_fa_qvs false prop in
  let () =
    Env.show_log "axiom_inline" @@ fun _ ->
    Printf.printf "setting: %s\n" (layout_setting setting)
  in
  (* let () = Printf.printf "faqvs: %s\n" (layout_setting faqvs) in *)
  let mapping = _safe_combine __FILE__ __LINE__ faqvs setting in
  let prop =
    List.fold_left
      (fun prop (x, y) -> subst_prop_instance x.x (AVar y) prop)
      prop mapping
  in
  alpha_renaming_ex_prop prop

let do_inline_ax_by_name ctx ax_name (faqvs, prop) =
  let ax = Hashtbl.find ctx.axs ax_name in
  (* let settings = find_inline_setting (faqvs @ exqvs) ax in *)
  let settings = find_inline_setting faqvs ax in
  let settings = filter_inlined ctx (ax_name, settings) in
  let () = update_inlined ctx (ax_name, settings) in
  let axs = List.map (fun s -> specialize_fa_qvs s ax) settings in
  smart_implies (smart_and axs) prop

let do_inline_over_pnf (ctx : inline_ctx) prop =
  let () =
    Env.show_log "axiom_inline" @@ fun _ ->
    Pp.printf "@{<bold>Prop:@} %s\n" (layout_prop prop)
  in
  let faqvs, prop = gather_fa_qvs false prop in
  let () =
    Env.show_log "axiom_inline" @@ fun _ ->
    Pp.printf "@{<bold>Forall qvs:@} %s\n" (List.to_string _get_x faqvs)
  in
  let () =
    Env.show_log "axiom_inline" @@ fun _ ->
    Pp.printf "@{<bold>Prop:@} %s\n" (layout_prop prop)
  in
  let prop =
    List.fold_left
      (fun prop name ->
        let prop = do_inline_ax_by_name ctx name (faqvs, prop) in
        let () =
          Env.show_log "axiom_inline" @@ fun _ ->
          Pp.printf "@{<bold>Prop [inline %s]:@} %s\n" name (layout_prop prop)
        in
        prop)
      prop ctx.ax_names
  in
  let prop = List.fold_right (fun qv body -> Forall { qv; body }) faqvs prop in
  (* let prop = contruct_prenex_normal_form (faqvs, exqvs) prop in *)
  let () =
    Env.show_log "axiom_inline" @@ fun _ ->
    Pp.printf "@{<bold>Inlined Result:@} %s\n" (layout_prop_to_coq prop)
  in
  prop

let inline_ax_with_bound iter_num axioms prop =
  if iter_num <= 0 then (axioms, prop)
  else
    let ctx, axioms = inline_ctx_init axioms in
    let rec aux i prop =
      if i <= 0 then prop else aux (i - 1) (do_inline_over_pnf ctx prop)
    in
    (axioms, aux iter_num prop)
