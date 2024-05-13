open Z3
open Z3.Expr
open Z3.Boolean
open Z3.Arithmetic
open Normalty.Ntyped
module T = Normalty.SMTtyped
open Sugar

let find_const_in_model m x =
  let cs = Z3.Model.get_const_decls m in
  let i =
    List.find_opt
      (fun d ->
        let name = Z3.Symbol.to_string @@ Z3.FuncDecl.get_name d in
        (* let () = Printf.printf "Find (%s) in %s\n" x name in *)
        String.equal name x)
      cs
  in
  match i with
  | Some i ->
      (* let () = Printf.printf "Const %s\n" @@ Z3.FuncDecl.to_string i in *)
      Some (Z3.FuncDecl.apply i [])
  | None -> None

let get_int_by_name m x =
  let i = find_const_in_model m x in
  match i with
  | None -> None
  | Some i -> (
      match Z3.Model.eval m i false with
      (* match Z3.Model.get_const_interp m i with *)
      | None -> _failatwith __FILE__ __LINE__ "get_int"
      | Some v ->
          (* Printf.printf "get_int(%s)\n" (Z3.Expr.to_string v); *)
          Some (int_of_string @@ Z3.Arithmetic.Integer.numeral_to_string v))

let get_string_by_name m x =
  let i = find_const_in_model m x in
  match i with
  | None -> None
  | Some i -> (
      match Z3.Model.eval m i false with
      (* match Z3.Model.get_const_interp m i with *)
      | None -> _failatwith __FILE__ __LINE__ "get_string"
      | Some v ->
          let str = Expr.to_string v in
          let str = List.of_seq @@ String.to_seq str in
          let str = List.filter (fun c -> not (Char.equal c '"')) str in
          let str = String.of_seq @@ List.to_seq str in
          (* Printf.printf "get_int(%s)\n" (Z3.Expr.to_string v); *)
          Some str)

let int_to_z3 ctx i = mk_numeral_int ctx i (Integer.mk_sort ctx)
let bool_to_z3 ctx b = if b then mk_true ctx else mk_false ctx

(* let tp_to_sort ctx t = *)
(*   (\* let () = *\) *)
(*   (\*   Printf.printf "z3aux t: %s\n" @@ Sexplib.Sexp.to_string @@ sexp_of_t t *\) *)
(*   (\* in *\) *)
(*   T.( *)
(*     match t with *)
(*     | Ty_uninter name -> Sort.mk_uninterpreted_s ctx name *)
(*     | _ -> ( *)
(*         match to_smtty t with *)
(*         | Int | Dt -> Integer.mk_sort ctx *)
(*         | Bool -> Boolean.mk_sort ctx)) *)

let tp_name_to_sort ctx tp =
  (* let () = Printf.printf "tp:%s\n" (Normalty.Frontend.layout tp) in *)
  T.(
    match tp with
    | Ty_uninter name -> Sort.mk_uninterpreted_s ctx name
    | _ -> (
        match to_smtty tp with
        | Dt -> Integer.mk_sort ctx
        (* | Dt -> *)
        (*     let tpname = *)
        (*       String.map (function ' ' -> '_' | '.' -> '_' | c -> c) *)
        (*       @@ Normalty.Frontend.layout tp *)
        (*     in *)
        (*     Sort.mk_uninterpreted_s ctx tpname *)
        | Int -> Integer.mk_sort ctx
        | Bool -> Boolean.mk_sort ctx))

let z3func ctx funcname inptps outtp =
  (* let () = Printf.printf "[%s]funcname: %s\n" __FILE__ funcname in *)
  FuncDecl.mk_func_decl ctx
    (Symbol.mk_string ctx funcname)
    (List.map (tp_name_to_sort ctx) inptps)
    (tp_name_to_sort ctx outtp)

let tpedvar_to_z3 ctx (tp, name) =
  T.(
    match tp with
    | Ty_uninter _ -> Expr.mk_const_s ctx name (tp_name_to_sort ctx tp)
    | _ -> (
        match to_smtty tp with
        | Dt -> Expr.mk_const_s ctx name (tp_name_to_sort ctx tp)
        | Int -> Integer.mk_const_s ctx name
        | Bool -> Boolean.mk_const_s ctx name))

let make_forall ctx qv body =
  if List.length qv == 0 then body
  else
    Quantifier.expr_of_quantifier
      (Quantifier.mk_forall_const ctx qv body (Some 1) [] [] None None)

let make_exists ctx qv body =
  if List.length qv == 0 then body
  else
    Quantifier.expr_of_quantifier
      (Quantifier.mk_exists_const ctx qv body (Some 1) [] [] None None)

let z3expr_to_bool v =
  match Boolean.get_bool_value v with
  | Z3enums.L_TRUE -> true
  | Z3enums.L_FALSE -> false
  | Z3enums.L_UNDEF -> failwith "z3expr_to_bool"
