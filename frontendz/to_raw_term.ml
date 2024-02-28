(* module MetaEnv = Env *)
open Ocaml5_parser
open Mutils
open Mtyped
open Parsetree
open Zzdatatype.Datatype
module Nt = Normalty.Frontend
open Raw_term
open Op
open To_id
open To_constant
open To_op
open Sugar

let typed_to_ocamlexpr f expr =
  match expr.ty with
  | None -> f expr.x
  | Some ty ->
      desc_to_ocamlexpr @@ Pexp_constraint (f expr.x, Nt.t_to_core_type ty)

(* let raw_term_to_ocamlexpr expr = *)
(*   let rec aux expr = *)
(*     let res = *)
(*       match expr.x with *)
(*       | Err -> Err *)
(*       | Tu es -> Tu (List.map aux es) *)
(*       | Var var -> Var var *)
(*       | Const c -> Const c *)
(*       | Let { if_rec; lhs; rhs; letbody } -> *)
(*           Let { if_rec; lhs; rhs = aux rhs; letbody = aux letbody } *)
(*       | AppOp (op, args) -> AppOp (op, List.map aux args) *)
(*       | App (func, args) -> App (aux func, List.map aux args) *)
(*       | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3) *)
(*       | Match (case_target, cs) -> *)
(*           let cs = *)
(*             List.map *)
(*               (fun { constructor; args; exp } -> *)
(*                 { constructor; args; exp = aux exp }) *)
(*               cs *)
(*           in *)
(*           Match (aux case_target, cs) *)
(*       | Lam { lamarg; lambody } -> Lam { lamarg; lambody = aux lambody } *)
(*     in *)
(*     res #: None *)
(*   in *)
(*   aux expr *)

let opt_typed_id_to_pattern id =
  let pat = string_to_pattern id.x in
  match id.ty with
  | None -> pat
  | Some ty -> typed_to_pattern (pat, Nt.t_to_core_type ty)

let opt_typed_ids_to_pattern ids =
  tuple_to_pattern (List.map opt_typed_id_to_pattern ids)

let rec typed_raw_term_to_ocamlexpr expr =
  typed_to_ocamlexpr raw_term_to_ocamlexpr expr

and raw_term_to_ocamlexpr expr =
  match expr with
  | Err -> mk_construct ("Err", [])
  | Tu es ->
      desc_to_ocamlexpr @@ Pexp_tuple (List.map typed_raw_term_to_ocamlexpr es)
  | Var var -> typed_to_ocamlexpr mkvar var
  | Const v -> constant_to_expr v
  | Let { if_rec; lhs; rhs; letbody } ->
      let flag = if if_rec then Asttypes.Recursive else Asttypes.Nonrecursive in
      let vb =
        mk_vb (opt_typed_ids_to_pattern lhs, typed_raw_term_to_ocamlexpr rhs)
      in
      desc_to_ocamlexpr
      @@ Pexp_let (flag, [ vb ], typed_raw_term_to_ocamlexpr letbody)
  | AppOp (op, args) ->
      (* NOTE: to make the printed code looks clearer, we don't print type of operators. *)
      mk_op_apply (layout_op op.x, List.map typed_raw_term_to_ocamlexpr args)
  | App (func, args) ->
      (* NOTE: to make the printed code looks clearer, we don't print type of function in its application. *)
      let func = raw_term_to_ocamlexpr func.x in
      let args =
        List.map
          (fun x -> (Asttypes.Nolabel, typed_raw_term_to_ocamlexpr x))
          args
      in
      desc_to_ocamlexpr @@ Pexp_apply (func, args)
  | Ite (e1, e2, e3) ->
      let e1, e2, e3 = map3 typed_raw_term_to_ocamlexpr (e1, e2, e3) in
      desc_to_ocamlexpr @@ Pexp_ifthenelse (e1, e2, Some e3)
  | Match { matched; match_cases } ->
      desc_to_ocamlexpr
      @@ Pexp_match
           ( typed_raw_term_to_ocamlexpr matched,
             List.map match_case_to_ocamlexpr match_cases )
  | Lam { lamarg; lambody } ->
      mklam
        (opt_typed_id_to_pattern lamarg)
        (typed_raw_term_to_ocamlexpr lambody)

and match_case_to_ocamlexpr = function
  | Matchcase { constructor; args; exp } ->
      (* NOTE: to make the printed code looks clearer, we don't print type of data constructors. *)
      let pc_lhs =
        string_dataconstr_to_pattern
          (constructor.x, List.map (fun x -> string_to_pattern x.x) args)
      in
      { pc_lhs; pc_guard = None; pc_rhs = typed_raw_term_to_ocamlexpr exp }

open Sugar

(* let id_to_var id = (fun x -> Var x) #-> id *)

let update_ty { x; ty } ty' =
  match ty with None -> x #: (Some ty') | Some _ -> x #: (Some ty')

type 't term_or_op =
  | C_is_term of ('t, 't raw_term) typed
  | C_is_op of ('t, op) typed

let constructor_to_term_or_op c =
  match c with
  | "Err" -> C_is_term Err #: (Some Nt.T.Ty_any)
  | "true" | "false" | "()" ->
      C_is_term { x = Const (string_to_constant c); ty = None }
  | name -> (
      match string_to_op_opt name with
      | None -> _failatwith __FILE__ __LINE__ "die: pat"
      | Some op -> C_is_op op #: None)

let typed_raw_term_of_ocamlexpr expr =
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_tuple es -> (Tu (List.map aux es)) #: None
    | Pexp_constraint (expr, ty) -> update_ty (aux expr) (Nt.core_type_to_t ty)
    | Pexp_ident id -> (Var (longid_to_id id) #: None) #: None
    | Pexp_construct (c, args) -> (
        let args =
          match args with
          | None -> []
          | Some args -> (
              let args = aux args in
              match args.x with Tu es -> es | _ -> [ args ])
        in
        let c = constructor_to_term_or_op @@ longid_to_id c in
        match c with
        | C_is_term tm -> tm
        | C_is_op op -> (AppOp (op, args)) #: None)
    | Pexp_constant _ -> (Const (expr_to_constant expr)) #: None
    | Pexp_let (flag, vbs, e) ->
        List.fold_right
          (fun vb letbody ->
            (Let
               {
                 if_rec = get_if_rec flag;
                 lhs = To_pat.patten_to_typed_ids vb.pvb_pat;
                 rhs = aux vb.pvb_expr;
                 letbody;
               })
            #: None)
          vbs (aux e)
    | Pexp_apply (func, args) -> (
        let func = aux func in
        match func.x with
        | Var name when String.equal name kw_perform -> (
            match args with
            | [ (_, arg) ] -> (
                let res = aux arg in
                match res.x with
                | AppOp (_, _) -> res
                | _ -> _failatwith __FILE__ __LINE__ "Syntax Error: perform")
            | _ -> _failatwith __FILE__ __LINE__ "Syntax Error: perform")
        | Var name when String.equal name kw_builtin -> (
            match args with
            | [ (_, arg) ] -> (
                let res = aux arg in
                match res.x with
                | App ({ x = Var op; ty }, args) ->
                    (AppOp ({ x = Op.BuiltinOp op; ty }, args)) #: res.ty
                | _ -> _failatwith __FILE__ __LINE__ "Syntax Error: builtin")
            | _ -> _failatwith __FILE__ __LINE__ "Syntax Error: builtin")
        | _ -> (App (func, List.map (fun x -> aux @@ snd x) args)) #: None)
    | Pexp_ifthenelse (e1, e2, Some e3) ->
        (Ite (aux e1, aux e2, aux e3)) #: None
    | Pexp_ifthenelse (e1, e2, None) ->
        (Ite (aux e1, aux e2, (Const Constant.U) #: unit_ty)) #: None
    | Pexp_match (case_target, cases) ->
        let cs =
          List.map
            (fun case ->
              match (To_pat.pattern_to_term case.pc_lhs).x with
              | AppOp ({ x = Op.DtOp op; ty }, args) ->
                  {
                    constructor = { x = op; ty };
                    args = List.map to_var args;
                    exp = aux case.pc_rhs;
                  }
              | _ -> _failatwith __FILE__ __LINE__ "?")
            cases
        in
        (Match (aux case_target, cs)) #: None
    | Pexp_fun (_, _, arg0, expr) ->
        let arg = To_pat.pattern_to_term arg0 in
        let () =
          match arg.ty with
          | None ->
              failwith
                (spf "Syntax error: lambda function should provide types (%s)"
                   (To_pat.layout_ arg0))
          | Some _ -> ()
        in
        let lamarg =
          match arg.x with
          | Var x -> x #: arg.ty
          | _ ->
              let () = Printf.printf "%s\n" (To_pat.layout_ arg0) in
              failwith "Syntax error: lambda function wrong argument"
        in
        (Lam { lamarg; lambody = aux expr }) #: None
        (* un-curry *)
    | Pexp_sequence (e1, e2) ->
        let lhs = [ { x = Rename.unique "unused"; ty = unit_ty } ] in
        (* let rhs = { x = (aux e1).x; ty = Nt.unit_ty } in *)
        let rhs = aux e1 in
        let letbody = aux e2 in
        (Let { if_rec = false; lhs; rhs; letbody }) #: None
    | _ ->
        raise
        @@ failwith
             (Sugar.spf "not imp client parsing:%s"
             @@ Pprintast.string_of_expression expr)
  in
  aux expr

let typed_id_of_ocamlexpr expr =
  let x = raw_term_of_ocamlexpr expr in
  match x.x with
  | Var id -> id #: x.ty
  | _ ->
      _failatwith __FILE__ __LINE__
        (spf "die: %s" (Pprintast.string_of_expression expr))

let id_of_ocamlexpr expr = (typed_id_of_ocamlexpr expr).x
let layout x = Pprintast.string_of_expression @@ raw_term_to_ocamlexpr x
let layout_omit_type x = layout @@ raw_term_to_ocamlexpr x

(* let prim_dt = [ "[]"; "::" ] *)
(* let is_prim_dt x = List.exists (String.equal x) prim_dt *)

(* let op_of_string_opt x = try Some (op_of_string x) with _ -> None *)
