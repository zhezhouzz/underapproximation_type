open Z3
open Sugar

type result = { ifcases : (Expr.expr * bool) list; elsecase : Expr.expr * bool }

open Zzdatatype.Datatype
module NT = Normalty.Ntyped
module Ntyped = Normalty.Ntyped
open Ntyped

let get_preds_interp model =
  let funcs = Model.get_func_decls model in
  let get func =
    (* let () = Printf.printf ">> %s\n" (FuncDecl.to_string func) in *)
    match Model.get_func_interp model func with
    | None -> _failatwith __FILE__ __LINE__ "never happen"
    | Some interp ->
        (* let func_name = Symbol.to_string @@ FuncDecl.get_name func in *)
        (* HACK *)
        let func_name =
          List.nth (String.split_on_char ' ' (FuncDecl.to_string func)) 1
        in
        let arity = Model.FuncInterp.get_arity interp in
        let entries = Model.FuncInterp.get_entries interp in
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
        (*   Printf.printf "%s(%i): %s\n" func_name arity (Expr.to_string e) *)
        (* in *)
        (* let str = "(= (k!2104 (:var 1)) (- 3))" in *)
        (* let () = Printf.printf "%s\n" str in *)
        (* let f = Parse.parse_string str in *)
        let f = Parse.parse_string @@ Expr.to_string e in
        (func_name, arity, f)
  in
  let m =
    List.fold_left
      (fun m (name, a, f) -> StrMap.add name (a, f) m)
      StrMap.empty (List.map get funcs)
  in
  let space =
    Parse.pasts_var_space @@ snd @@ List.split @@ StrMap.to_value_list m
  in
  (m, space, space)

(* let pre_pred_tab = StrMap.from_kv_list [ ("==", fun arr -> arr.(0) == arr.(1)) ] *)

let get_fvs features vars qvs model =
  let vars_interp =
    List.map (fun x -> (x, Z3aux.get_int_by_name model x)) vars
  in
  let ftab, dts, space = get_preds_interp model in
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
  let fv_tab = Hashtbl.create 100 in
  let aux fv_tab_tmp var_m qvs_v =
    let m = StrMap.add_seq (List.to_seq @@ List.combine qvs qvs_v) var_m in
    let fv =
      List.map
        (fun (mp, args) ->
          let args =
            List.map
              (fun x -> StrMap.find (spf "find arg(%s) fail" x.x) m x.x)
              args
          in
          match (mp, args) with
          | "==", [ a1; a2 ] -> a1 == a2
          | "<=", [ a1; a2 ] -> a1 <= a2
          | "<", [ a1; a2 ] -> a1 < a2
          | _, _ -> Parse.exec_bool ftab mp args)
        features
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
