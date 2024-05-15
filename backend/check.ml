open Z3
open Solver
open Goal
(* open Z3aux *)

type smt_result = SmtSat of Model.model | SmtUnsat | Timeout

let solver_result solver =
  (* let _ = printf "solver_result\n" in *)
  match check solver [] with
  | UNSATISFIABLE -> SmtUnsat
  | UNKNOWN ->
      (* raise (InterExn "time out!") *)
      (* Printf.printf "\ttimeout\n"; *)
      Timeout
  | SATISFIABLE -> (
      match Solver.get_model solver with
      | None -> failwith "never happen"
      | Some m -> SmtSat m)

let get_int m i =
  match Model.eval m i true with
  | None -> failwith "get_int"
  | Some v ->
      (* printf "get_int(%s)\n" (Expr.to_string i); *)
      int_of_string @@ Arithmetic.Integer.numeral_to_string v

let get_bool_str m i =
  match Model.eval m i true with None -> "none" | Some v -> Expr.to_string v

let get_int_name ctx m name =
  get_int m @@ Arithmetic.Integer.mk_const_s ctx name

let get_pred m predexpr =
  match Model.eval m predexpr true with
  | None -> failwith "get pred"
  | Some v -> Z3aux.z3expr_to_bool v

let get_unknown_fv ctx m unknown_fv =
  List.map (fun (_, b) -> get_pred m (Boolean.mk_const_s ctx b)) unknown_fv

let smt_solve ctx assertions =
  (* let _ = printf "check\n" in *)
  let solver = mk_solver ctx None in
  let g = mk_goal ctx true false false in
  (* let () = Printf.printf "Q: %s\n" @@ Frontend.coq_layout vc in *)
  (* let () = failwith "zz" in *)
  (* let () = exit 0 in *)
  let _ = Goal.add g assertions in
  (* let g = Goal.simplify g None in *)
  (* let g = *)
  (*   Tactic.(ApplyResult.get_subgoal (apply (mk_tactic ctx "snf") g None) 0) *)
  (* in *)
  (* let () = *)
  (*   Printf.printf "Goal: %s\n\n" *)
  (*   @@ Zzdatatype.Datatype.List.split_by "\n" Expr.to_string *)
  (*   @@ Goal.get_formulas g *)
  (* in *)
  let _ = Solver.add solver (get_formulas g) in
  (* let () = *)
  (*   Printf.printf "Tactics: %s\n" *)
  (*     (Zzdatatype.Datatype.StrList.to_string (Z3.Tactic.get_tactic_names ctx)) *)
  (* in *)
  (* let res = Z3.Tactic.apply (Z3.Tactic.mk_tactic ctx "nnf") g None in *)
  (* let () = Printf.printf "%s\n" (Z3.Tactic.ApplyResult.to_string res) in *)
  (* let gs = Z3.Tactic.ApplyResult.get_subgoals res in *)
  (* let _ = List.iter (fun g -> Solver.add solver (get_formulas g)) gs in *)
  let _, res = Sugar.clock (fun () -> solver_result solver) in
  res

let extend =
  [
    ("len", [ "hd"; "tl"; "emp" ]);
    ("uniq", [ "hd"; "tl"; "emp" ]);
    ("bst", [ "num_node"; "botright" ]);
    ( "typing",
      [
        "is_const";
        "is_var";
        "is_abs";
        "is_app";
        "num_app";
        "stlc_ty_nat";
        "stlc_ty_arr1";
        "stlc_ty_arr2";
        "stlc_const";
        "stlc_id";
        "stlc_app1";
        "stlc_app2";
        "stlc_abs_ty";
        "stlc_abs_body";
        "stlc_tyctx_emp";
        "stlc_tyctx_hd";
        "stlc_tyctx_tl";
      ] );
  ]

let _filter_ax = true
let _inline_ax_iter_bound = 1

let smt_neg_and_solve ctx (axioms : (string * Nt.t Language.prop) list) vc =
  (* let () = *)
  (*   Env.show_debug_queries @@ fun _ -> *)
  (*   Printf.printf "Query: %s\n" @@ Language.Rty.layout_prop vc *)
  (* in *)
  let open Language in
  let current_mps = prop_get_mp vc in
  let current_mps =
    List.concat
    @@ List.map
         (fun mp ->
           match
             List.find_opt (fun (name, _) -> String.equal name mp) extend
           with
           | Some (_, res) -> mp :: res
           | _ -> [ mp ])
         current_mps
  in
  (* let _ = *)
  (*   Printf.printf "current_mps: %s\n" *)
  (*     (Zzdatatype.Datatype.StrList.to_string current_mps) *)
  (* in *)
  let axioms =
    if _filter_ax then
      List.filter
        (fun (_, a) ->
          let mps = prop_get_mp a in
          List.for_all (fun mp -> List.exists (String.equal mp) current_mps) mps)
        axioms
    else axioms
  in
  let () =
    Env.show_log "axiom" @@ fun _ ->
    Printf.printf "Num of axioms before inline: %i\n" (List.length axioms)
  in
  (* let () = failwith "end" in *)
  let axioms, vc =
    Ax_inline.inline_ax_with_bound _inline_ax_iter_bound axioms vc
  in
  let () =
    Env.show_log "axiom_inline" @@ fun _ ->
    Printf.printf "Num of axioms after inline: %i\n" (List.length axioms)
  in
  let () =
    Env.show_log "axiom_inline" @@ fun _ ->
    List.iter
      (fun (name, prop) ->
        Pp.printf "@{<yellow>Ax: %s@} := %s\n" name (layout_prop prop))
      axioms
  in
  let () =
    Env.show_log "axiom_inline" @@ fun _ ->
    Pp.printf "@{<yellow>VC@} := %s\n" (layout_prop_to_coq vc)
  in
  let assertions =
    List.map (Propencoding.to_z3 ctx) (List.map snd axioms @ [ Not vc ])
  in
  let time_t, res =
    Sugar.clock (fun () ->
        match smt_solve ctx [ Propencoding.to_z3 ctx (Not vc) ] with
        | SmtUnsat -> SmtUnsat
        | _ -> smt_solve ctx assertions)
  in
  let () =
    Env.show_debug_stat @@ fun _ -> Pp.printf "Z3 solving time: %0.4fs\n" time_t
  in
  res

exception SMTTIMEOUT

let debug_counter = ref 0
let smt_timeout_flag = ref false

(** Unsat means true; otherwise means false *)
let handle_check_res query_action =
  let time_t, res = Sugar.clock query_action in
  let () =
    Env.show_debug_stat @@ fun _ ->
    Pp.printf "@{<bold>Solving time: %.2f@}\n" time_t
  in
  (* let () = *)
  (*   if 18 == !debug_counter then failwith "end" *)
  (*   else debug_counter := !debug_counter + 1 *)
  (* in *)
  smt_timeout_flag := false;
  match res with
  | SmtUnsat -> true
  | SmtSat _ -> false
  (* | SmtSat model -> *)
  (*     ( Env.show_log "model" @@ fun _ -> *)
  (*       Printf.printf "model:\n%s\n" *)
  (*       @@ Sugar.short_str 10000 @@ Z3.Model.to_string model ); *)
  (*     false *)
  | Timeout ->
      ( Env.show_debug_queries @@ fun _ ->
        Pp.printf "@{<bold>@{<orange>SMTTIMEOUT@}@}\n" );
      smt_timeout_flag := true;
      false
