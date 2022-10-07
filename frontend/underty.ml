open Ocaml_parser
open Parsetree
open Zzdatatype.Datatype
module T = Ast.Termlang
module L = Ast.UT
module Ntyped = Ast.Ntyped
module Type = Normalty.Frontend
open Sugar

type mode = Under | Tuple

open L

let ot_pretty_layout x =
  match x with
  | { basename; prop = Autov.Prop.(MethodPred ("==", [ AVar id; ACint i ])); _ }
    when String.equal basename id.x ->
      Sugar.spf "{%i}" i
  | {
   basename;
   prop = Autov.Prop.(MethodPred ("==", [ AVar id; ACbool b ]));
   _;
  }
    when String.equal basename id.x ->
      Sugar.spf "{%b}" b
  | { basename; normalty; prop } ->
      Sugar.spf "{%s:%s | %s}" basename (Type.layout normalty)
        (Autov.pretty_layout_prop prop)

let pretty_layout x =
  let rec aux x =
    match x with
    | UnderTy_base
        {
          basename;
          prop = Autov.Prop.(MethodPred ("==", [ AVar id; ACint i ]));
          _;
        }
      when String.equal basename id.x ->
        Sugar.spf "[%i]" i
    | UnderTy_base
        {
          basename;
          prop = Autov.Prop.(MethodPred ("==", [ AVar id; ACbool b ]));
          _;
        }
      when String.equal basename id.x ->
        Sugar.spf "[%b]" b
    | UnderTy_base { basename; normalty; prop } ->
        Sugar.spf "[%s:%s | %s]" basename (Type.layout normalty)
          (Autov.pretty_layout_prop prop)
    | UnderTy_under_arrow { argty; retty } ->
        let arg_str = aux argty in
        let arg_str =
          if Ast.UT.is_base_type argty then arg_str else spf "(%s)" arg_str
        in
        Sugar.spf "%s→%s" arg_str (aux retty)
    | UnderTy_ghost_arrow { argname; argty; retty } ->
        Sugar.spf "%s⤍%s"
          (spf "%s:%s" argname (ot_pretty_layout argty))
          (aux retty)
    | UnderTy_over_arrow { argname; argty; retty } ->
        Sugar.spf "%s→%s"
          (spf "%s:%s" argname (ot_pretty_layout argty))
          (aux retty)
    | UnderTy_tuple ts ->
        (* let () = Printf.printf "len(ts) = %i\n" @@ List.length ts in *)
        Sugar.spf "(%s)" @@ Zzdatatype.Datatype.List.split_by_comma aux ts
  in
  aux x

let prop_of_ocamlexpr e =
  let prop = Autov.prop_of_ocamlexpr e in
  let _ = Autov.Prop.assume_tope_uprop __FILE__ __LINE__ prop in
  prop

(* let mode_of_ocamlexpr e = *)
(*   match (Expr.expr_of_ocamlexpr e).x with *)
(*   | T.Var "under" -> Under *)
(*   | T.Var "tuple" -> Tuple *)
(*   | _ -> failwith "mode_of_ocamlexpr" *)

let ot_undertype_of_ocamlexpr expr =
  match expr.pexp_desc with
  | Pexp_constraint (e, ct) ->
      let basename, normalty =
        match Type.core_type_to_notated_t ct with
        | Some basename, normalty -> (basename, normalty)
        | _ -> _failatwith __FILE__ __LINE__ ""
      in
      let prop = prop_of_ocamlexpr e in
      { basename; normalty; prop }
  | _ -> _failatwith __FILE__ __LINE__ ""

let undertype_of_ocamlexpr expr =
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_tuple es -> L.UnderTy_tuple (List.map aux es)
    | Pexp_let (_, [ vb ], body) -> (
        let id = Pat.patten_to_typed_ids vb.pvb_pat in
        let argname =
          match id with [ id ] -> id | _ -> failwith "undertype_of_ocamlexpr"
        in
        match argname.ty with
        | Some (Some "over", argnty) ->
            let argty = ot_undertype_of_ocamlexpr vb.pvb_expr in
            let _ =
              _check_equality __FILE__ __LINE__ Ast.NT.eq argnty argty.normalty
            in
            L.UnderTy_over_arrow
              { argname = argname.x; argty; retty = aux body }
        | Some (Some "ghost", argnty) ->
            let argty = ot_undertype_of_ocamlexpr vb.pvb_expr in
            let _ =
              _check_equality __FILE__ __LINE__ Ast.NT.eq argnty argty.normalty
            in
            L.UnderTy_ghost_arrow
              { argname = argname.x; argty; retty = aux body }
        | Some (Some "under", argnty) ->
            let argty = aux vb.pvb_expr in
            let _ =
              _check_equality __FILE__ __LINE__ Ast.NT.eq argnty (erase argty)
            in
            let _ =
              try
                _check_equality __FILE__ __LINE__ String.equal "dummy" argname.x
              with e ->
                Printf.printf
                  "\n\
                   The unused varaible should use name 'dummy', instead of %s\n\n"
                  argname.x;
                raise e
            in
            L.UnderTy_under_arrow { argty; retty = aux body }
        | _ ->
            _failatwith __FILE__ __LINE__
              (match argname.ty with
              | None -> "none"
              | Some ty ->
                  spf "%s: %s"
                    (match fst ty with None -> "none" | Some s -> s)
                    (Type.layout @@ snd ty)))
    | Pexp_constraint (e, ct) ->
        let basename, normalty =
          match Type.core_type_to_notated_t ct with
          | Some basename, normalty -> (basename, normalty)
          | _ -> _failatwith __FILE__ __LINE__ ""
        in
        let prop = prop_of_ocamlexpr e in
        L.(UnderTy_base { basename; normalty; prop })
    | _ ->
        _failatwith __FILE__ __LINE__
          (spf "wrong refinement type: %s"
             (Pprintast.string_of_expression expr))
    (* and aux_with_name expr = *)
    (*   match expr.pexp_desc with *)
    (*   | Pexp_apply (x, [ e ]) -> *)
    (*       let ty = aux @@ snd e in *)
    (*       let x = *)
    (*         match Expr.expr_of_ocamlexpr x with *)
    (*         | { x = Var x; _ } -> x *)
    (*         | _ -> _failatwith __FILE__ __LINE__ "" *)
    (*       in *)
    (*       (x, ty) *)
    (*   | _ -> _failatwith __FILE__ __LINE__ "" *)
  in
  let uty = aux expr in
  (* let () = Printf.printf "TEST:%s\n" @@ pretty_layout uty in *)
  uty

let undertype_to_ocamlexpr _ = _failatwith __FILE__ __LINE__ "unimp"
let layout x = Pprintast.string_of_expression @@ undertype_to_ocamlexpr x
