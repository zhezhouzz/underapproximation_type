open Ocaml5_parser
open Parsetree
(* open Mtyped *)

(* open Mutils *)
open Zzdatatype.Datatype
module Nt = Normalty.Frontend
open Rty
open To_cty
open To_id
open Sugar

let rec layout_rty = function
  | RtyBase { ou; cty } -> (
      match ou_to_qt ou with
      | Normalty.Connective.Fa -> spf "{%s}" (layout_cty cty)
      | Normalty.Connective.Ex -> spf "[%s]" (layout_cty cty))
  | RtyBaseArr { argcty; arg; retty } -> (
      match arg with
      | "_" -> spf "{%s} → %s" (layout_cty argcty) (layout_rty retty)
      | _ -> spf "(%s:{%s}) → %s" arg (layout_cty argcty) (layout_rty retty))
  | RtyArrArr { argrty; retty } ->
      spf "%s → %s" (layout_rty argrty) (layout_rty retty)
  | RtyTuple ts -> spf "(%s)" @@ List.split_by_comma layout_rty ts

let get_ou expr =
  match expr.pexp_attributes with
  | l when List.exists (fun x -> String.equal x.attr_name.txt "over") l -> true
  | _ -> false

let rec rty_of_expr expr =
  match expr.pexp_desc with
  | Pexp_constraint _ ->
      let cty = cty_of_expr expr in
      if get_ou expr then RtyBase { ou = true; cty }
      else RtyBase { ou = false; cty }
  | Pexp_fun (_, rtyexpr, pattern, body) -> (
      let retty = rty_of_expr body in
      let arg = id_of_pattern pattern in
      match rtyexpr with
      | None -> _failatwith __FILE__ __LINE__ "die"
      | Some rtyexpr -> (
          match rty_of_expr rtyexpr with
          | RtyBase { cty; _ } -> RtyBaseArr { argcty = cty; arg; retty }
          | RtyTuple _ -> _failatwith __FILE__ __LINE__ "die"
          | argrty -> RtyArrArr { argrty; retty }))
  | Pexp_let (_, [ vb ], body) -> (
      let retty = rty_of_expr body in
      let arg = id_of_pattern vb.pvb_pat in
      match rty_of_expr vb.pvb_expr with
      | RtyBase { cty; _ } -> RtyBaseArr { argcty = cty; arg; retty }
      | RtyTuple _ -> _failatwith __FILE__ __LINE__ "die"
      | argrty -> RtyArrArr { argrty; retty })
  | Pexp_tuple es -> RtyTuple (List.map rty_of_expr es)
  | _ ->
      _failatwith __FILE__ __LINE__
        (spf "wrong refinement type: %s" (Pprintast.string_of_expression expr))
