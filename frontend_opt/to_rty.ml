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
open Normalty.Connective

let rec layout_rty = function
  | RtyBase { ou = Fa; cty } -> spf "{%s}" (layout_cty cty)
  | RtyBase { ou = Ex; cty } -> spf "[%s]" (layout_cty cty)
  | RtyBaseArr { argcty; arg; retty } -> (
      match arg with
      | "_" -> spf "{%s} → %s" (layout_cty argcty) (layout_rty retty)
      | _ -> spf "(%s:{%s}) → %s" arg (layout_cty argcty) (layout_rty retty))
  | RtyGhostArr { argnty; arg; retty } ->
      (* spf "(%s:{%s}) ⇢ %s" arg (layout_cty argcty) (layout_rty retty) *)
      spf "%s:%s ⇢ %s" arg (Nt.layout argnty) (layout_rty retty)
  | RtyBaseDepPair { argcty; arg; retty } -> (
      match arg with
      | "_" -> spf "[%s] → %s" (layout_cty argcty) (layout_rty retty)
      | _ -> spf "(%s:[%s]) → %s" arg (layout_cty argcty) (layout_rty retty))
  | RtyArrArr { argrty; retty } ->
      spf "%s → %s" (layout_rty argrty) (layout_rty retty)
  | RtyInter (rty1, rty2) -> spf "%s ⊓ %s" (layout_rty rty1) (layout_rty rty2)

let get_ou expr =
  match expr.pexp_attributes with
  | l when List.exists (fun x -> String.equal x.attr_name.txt "over") l -> Fa
  | _ -> Ex

let get_ghost pat =
  match pat.ppat_attributes with
  | l when List.exists (fun x -> String.equal x.attr_name.txt "ghost") l -> true
  | _ -> false

let rec rty_of_expr expr =
  match expr.pexp_desc with
  | Pexp_constraint _ -> RtyBase { ou = get_ou expr; cty = cty_of_expr expr }
  | Pexp_fun (_, rtyexpr, pattern, body) -> (
      let retty = rty_of_expr body in
      match rtyexpr with
      | None -> (
          match pattern.ppat_desc with
          | Ppat_constraint (x, ty) ->
              let arg = id_of_pattern x in
              let argnty = Nt.core_type_to_t ty in
              RtyGhostArr { argnty; arg; retty }
          | _ -> _failatwith __FILE__ __LINE__ "die")
      | Some rtyexpr -> (
          let arg = id_of_pattern pattern in
          match rty_of_expr rtyexpr with
          | RtyBase { cty; ou = Fa } ->
              (* if get_ghost pattern then RtyGhostArr { argcty = cty; arg; retty } *)
              (* else *)
              RtyBaseArr { argcty = cty; arg; retty }
          | RtyBase { ou = Ex; _ } -> _failatwith __FILE__ __LINE__ "die"
          | RtyInter _ -> _failatwith __FILE__ __LINE__ "die"
          | argrty -> RtyArrArr { argrty; retty }))
  | Pexp_let (_, [ vb ], body) -> (
      let retty = rty_of_expr body in
      let arg = id_of_pattern vb.pvb_pat in
      match rty_of_expr vb.pvb_expr with
      | RtyBase { cty; ou = Fa } -> RtyBaseArr { argcty = cty; arg; retty }
      | RtyBase { cty; ou = Ex } -> RtyBaseDepPair { argcty = cty; arg; retty }
      | RtyInter _ -> _failatwith __FILE__ __LINE__ "die"
      | _ -> _failatwith __FILE__ __LINE__ "die")
  | Pexp_array ls -> (
      let htys = List.map rty_of_expr ls in
      match List.rev htys with
      | [] | [ _ ] -> failwith "syntax error: empty/singleton intersection type"
      | rty :: rtys ->
          List.fold_right
            (fun rty res -> RtyInter (rty, res))
            (List.rev rtys) rty)
  | _ ->
      _failatwith __FILE__ __LINE__
        (spf "wrong refinement type: %s" (Pprintast.string_of_expression expr))
