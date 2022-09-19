open Z3
open Sugar

type result = { ifcases : (Expr.expr * bool) list; elsecase : Expr.expr * bool }

open Zzdatatype.Datatype
module NT = Normalty.Ast.T
module Ntyped = Normalty.Ast.Ntyped
open Ntyped

let get_preds_interp model =
  let funcs = Model.get_func_decls model in
  let get func =
    (* let () = Printf.printf ">> %s\n" (FuncDecl.to_string func) in *)
    match Model.get_func_interp model func with
    | None -> _failatwith __FILE__ __LINE__ "never happen"
    | Some interp ->
        let func_name = Symbol.to_string @@ FuncDecl.get_name func in
        let arity = Model.FuncInterp.get_arity interp in
        let handle e =
          let open Model.FuncInterp.FuncEntry in
          let b = Z3aux.z3expr_to_bool (get_value e) in
          let args = get_args e in
          let argsname = List.map Expr.to_string args in
          let () =
            Printf.printf "(%s) --> %b\n" (StrList.to_string argsname) b
          in
          ()
        in
        let entries = List.map handle @@ Model.FuncInterp.get_entries interp in
        let _ =
          if List.length entries > 0 then _failatwith __FILE__ __LINE__ ""
          else ()
        in
        let e = Model.FuncInterp.get_else interp in
        (* let args, prope = Parse.parse_to_prop (arity, Expr.to_string e) in *)
        (* let () = *)
        (*   Printf.printf "get_entries[%s] (%i)\n" func_name *)
        (*     (List.length (Model.FuncInterp.get_entries interp)) *)
        (* in *)
        (* let () = *)
        (*   Printf.printf "else[%s]: %s\n" *)
        (*     (List.split_by_comma (fun x -> x.Ntyped.x) args) *)
        (*     (Frontend.pretty_layout prope) *)
        (* in *)
        let space, f = Parse.parse_to_func (arity, Expr.to_string e) in
        (* let () = *)
        (*   List.iter (fun l -> *)
        (*       Printf.printf "%s(%s) := %b\n" func_name *)
        (*         (List.split_by_comma string_of_int l) *)
        (*         (f (Array.of_list l))) *)
        (*   @@ List.choose_list_list space *)
        (* in *)
        (* HACK: add dt to space *)
        let dts = List.nth space 0 in
        let space = List.concat space in
        ((func_name, f), (dts, space))
  in
  let tab, space = List.split @@ List.map get funcs in
  let dts, space = List.split space in
  ( tab,
    List.slow_rm_dup ( == ) @@ List.concat dts,
    List.slow_rm_dup ( == ) @@ List.concat space )

let pre_pred_tab = StrMap.from_kv_list [ ("==", fun arr -> arr.(0) == arr.(1)) ]

let get_fvs features vars qvs model =
  let vars_interp =
    List.map (fun x -> (x, Z3aux.get_int_by_name model x)) vars
  in
  let preds_interp, dts, space = get_preds_interp model in
  let names, values = List.split vars_interp in
  let var_ms =
    let m =
      List.map (fun x -> match x with None -> dts | Some y -> [ y ]) values
    in
    let settings = List.choose_list_list m in
    List.map StrMap.from_kv_list @@ List.map (List.combine names) settings
  in
  let space =
    List.slow_rm_dup ( == ) (List.filter_map (fun x -> x) values @ space)
  in
  let pred_tab = StrMap.add_seq (List.to_seq preds_interp) pre_pred_tab in
  let features' =
    List.map
      (fun (f, args) ->
        (StrMap.find "get pred fail" pred_tab f, List.map (fun x -> x.x) args))
      features
  in
  (* let () = Printf.printf "space : %s\n" (IntList.to_string space) in *)
  let fv_tab = Hashtbl.create 100 in
  let aux fv_tab_tmp var_m qvs_v =
    let m = StrMap.add_seq (List.to_seq @@ List.combine qvs qvs_v) var_m in
    let fv =
      List.map
        (fun (mp, args) ->
          let args =
            List.map (fun x -> StrMap.find (spf "find arg(%s) fail" x) m x) args
          in
          mp (Array.of_list args))
        features'
    in
    (* let () = *)
    (*   Printf.printf "(%s) --> [%s]\n" *)
    (*     (List.split_by_comma (fun (x, i) -> spf "%s:%i" x i) *)
    (*     @@ StrMap.to_kv_list m) *)
    (*     (List.split_by_comma string_of_bool fv) *)
    (* in *)
    match Hashtbl.find_opt fv_tab_tmp fv with
    | None -> Hashtbl.add fv_tab_tmp fv ()
    | Some _ -> ()
  in
  let aux var_m qvs =
    let fv_tab_tmp = Hashtbl.create 100 in
    let () = List.iter (fun qvs_v -> aux fv_tab_tmp var_m qvs_v) qvs in
    Hashtbl.iter
      (fun fv _ ->
        match Hashtbl.find_opt fv_tab fv with
        | None -> Hashtbl.add fv_tab fv 1
        | Some n -> Hashtbl.replace fv_tab fv (n + 1))
      fv_tab_tmp
  in
  let qvs = List.choose_list_list (List.map (fun _ -> space) qvs) in
  let () = List.iter (fun x -> aux x qvs) var_ms in
  List.of_seq @@ Hashtbl.to_seq @@ fv_tab
