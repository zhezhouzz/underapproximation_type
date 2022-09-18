module Prop = Prop
module Func_interp = Func_interp
module Smtty = Normalty.Ast.Smtty
module Enum = Enum

exception FailWithModel of string * Z3.Model.model

let _failwithmodel file line msg model =
  raise (FailWithModel (Printf.sprintf "[%s:%i] %s" file line msg, model))

let ctx =
  Z3.mk_context
    [ ("model", "true"); ("proof", "false"); ("timeout", "2999999") ]

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

exception SMTTIMEOUT

let _check pre q =
  let open Check in
  match smt_neg_and_solve ctx pre q with
  | SmtUnsat -> None
  | SmtSat model ->
      (* Printf.printf "model:\n%s\n" @@ Z3.Model.to_string model; *)
      (* pretty_print_model model; *)
      Some model
  | Timeout -> raise SMTTIMEOUT

open Sugar

let get_mp_app model (mp, args) =
  let mp =
    List.find
      (fun d ->
        let name = Z3.Symbol.to_string @@ Z3.FuncDecl.get_name d in
        (* let () = Printf.printf "Get: %s\n" name in *)
        String.equal name mp)
      (Z3.Model.get_func_decls model)
  in
  let args = List.map (Z3aux.find_const_in_model model) args in
  let args =
    List.map
      (function Some x -> x | None -> _failatwith __FILE__ __LINE__ "")
      args
  in
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
(* let peval = Simp.peval *)
