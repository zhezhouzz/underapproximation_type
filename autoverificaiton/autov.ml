module Prop = Prop.T
module Smtty = Normalty.Ast.Smtty
module Enum = Enum

exception FailWithModel of string * Z3.Model.model

let _failwithmodel file line msg model =
  raise (FailWithModel (Printf.sprintf "[%s:%i] %s" file line msg, model))

let ctx =
  Z3.mk_context [ ("model", "true"); ("proof", "false"); ("timeout", "1999") ]

let pretty_print_model model =
  Z3.Model.to_string model |> fun s ->
  List.fold_left
    (fun acc c ->
      Str.global_replace
        (Str.regexp
           ("\\([^ ] \\)"
           ^ (Z3.Model.get_const_interp model c
             |> Option.get |> Z3.Expr.to_string)))
        ("\\1" ^ (Z3.FuncDecl.get_name c |> Z3.Symbol.to_string))
        acc)
    s
    (Z3.Model.get_const_decls model)
  |> Printf.printf "%s\n"

let _check pres q =
  let open Check in
  match smt_neg_and_solve ctx pres q with
  | SmtUnsat -> None
  | SmtSat model ->
      Printf.printf "model:\n%s\n" @@ Z3.Model.to_string model;
      (* pretty_print_model model; *)
      Some model
  | Timeout -> failwith "smt timeout"

open Sugar

let find_const_in_model m x =
  let cs = Z3.Model.get_const_decls m in
  let i =
    List.find
      (fun d ->
        let name = Z3.Symbol.to_string @@ Z3.FuncDecl.get_name d in
        let () = Printf.printf "Find (%s) in %s\n" x name in
        String.equal name x)
      cs
  in
  Z3.FuncDecl.apply i []

let get_int_by_name m x =
  let i = find_const_in_model m x in
  match Z3.Model.eval m i false with
  (* match Z3.Model.get_const_interp m i with *)
  | None -> failwith "get_int"
  | Some v ->
      Printf.printf "get_int(%s)\n" (Z3.Expr.to_string v);
      int_of_string @@ Z3.Arithmetic.Integer.numeral_to_string v

let get_mp_app model (mp, args) =
  let mp =
    List.find
      (fun d ->
        let name = Z3.Symbol.to_string @@ Z3.FuncDecl.get_name d in
        (* let () = Printf.printf "Get: %s\n" name in *)
        String.equal name mp)
      (Z3.Model.get_func_decls model)
  in
  let args = List.map (find_const_in_model model) args in
  let prop_z3 = Z3.FuncDecl.apply mp args in
  match Z3.Model.eval model prop_z3 false with
  | None -> _failatwith __FILE__ __LINE__ ""
  | Some b ->
      let b =
        match Z3.Boolean.get_bool_value b with
        | Z3enums.L_TRUE -> true
        | Z3enums.L_FALSE -> false
        | Z3enums.L_UNDEF ->
            failwith @@ spf "get pred: %s" @@ Z3.Expr.to_string b
      in
      b

let check pres vc = _check pres vc
let check_implies pres a b = _check pres Prop.(Implies (a, b))
let check_implies_multi_pre pres a_s b = _check pres Prop.(Implies (And a_s, b))
let prop_of_ocamlexpr = Frontend.prop_of_ocamlexpr
let prop_to_ocamlexpr = Frontend.prop_to_expr
let layout_prop = Frontend.layout
let pretty_layout_prop = Frontend.pretty_layout
let pretty_layout_lit = Frontend.pretty_layout_lit
let coq_layout_prop = Frontend.coq_layout
let prop_fv = Prop.fv
let add_prop_to_fv = Prop.add_fv
let uqv_encoding = Encoding.uqv_encoding
let vars_reduction = Encoding.vars_reduction
