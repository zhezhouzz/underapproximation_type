open Ocaml5_parser
open Parsetree
open Syntax
open Zzdatatype.Datatype
open To_cty
open To_id
open Sugar

(* open Mutils *)
open Normalty.Connective

let rec layout_rty = function
  | RtyBase { ou; cty } -> (
      match ou with
      | Fa -> spf "{%s}" (layout_cty cty)
      | Ex -> spf "[%s]" (layout_cty cty))
  | RtyBaseArr { argcty; arg; retty } -> (
      match arg with
      | "_" -> spf "{%s} → %s" (layout_cty argcty) (layout_rty retty)
      | _ -> spf "(%s:{%s}) → %s" arg (layout_cty argcty) (layout_rty retty))
  | RtyGhostArr { argcty; arg; retty } ->
      spf "(%s:{%s}) ⇢ %s" arg (layout_cty argcty) (layout_rty retty)
  | RtyBaseDepPair { argcty; arg; retty } -> (
      match arg with
      | "_" -> spf "[%s] → %s" (layout_cty argcty) (layout_rty retty)
      | _ -> spf "(%s:[%s]) → %s" arg (layout_cty argcty) (layout_rty retty))
  | RtyArrArr { argrty; retty } ->
      spf "%s → %s" (layout_rty argrty) (layout_rty retty)

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
  (* | Pexp_tuple [ er; cty ] -> *)
  (*     let er = To_prop.prop_of_expr er in *)
  (*     RtyBase { ou = get_ou cty; cty = cty_of_expr cty; er } *)
  | Pexp_constraint _ -> RtyBase { ou = get_ou expr; cty = cty_of_expr expr }
  | Pexp_fun (_, rtyexpr, pattern, body) -> (
      let retty = rty_of_expr body in
      match rtyexpr with
      | None -> _failatwith __FILE__ __LINE__ "wrong format"
      | Some rtyexpr -> (
          let arg = id_of_pattern pattern in
          (* let () = Printf.printf "pattern: %s\n" (layout_ pattern) in *)
          match rty_of_expr rtyexpr with
          | RtyBase { cty; ou = Fa; _ } ->
              if get_ghost pattern then RtyGhostArr { argcty = cty; arg; retty }
              else RtyBaseArr { argcty = cty; arg; retty }
          | RtyBase { ou = Ex; _ } -> _failatwith __FILE__ __LINE__ "die"
          | argrty -> RtyArrArr { argrty; retty }))
  | Pexp_let (_, [ vb ], body) -> (
      let retty = rty_of_expr body in
      let arg = id_of_pattern vb.pvb_pat in
      match rty_of_expr vb.pvb_expr with
      | RtyBase { cty; ou = Fa; _ } ->
          if get_ghost vb.pvb_pat then RtyGhostArr { argcty = cty; arg; retty }
          else RtyBaseArr { argcty = cty; arg; retty }
      | RtyBase { cty; ou = Ex; _ } ->
          RtyBaseDepPair { argcty = cty; arg; retty }
      | _ -> _failatwith __FILE__ __LINE__ "die")
  (* | Pexp_array ls -> ( *)
  (*     let htys = List.map rty_of_expr ls in *)
  (*     match List.rev htys with *)
  (*     | [] | [ _ ] -> failwith "syntax error: empty/singleton intersection type" *)
  (*     | rty :: rtys -> *)
  (*         List.fold_right *)
  (*           (fun rty res -> RtyInter (rty, res)) *)
  (*           (List.rev rtys) rty) *)
  | _ ->
      _failatwith __FILE__ __LINE__
        (spf "wrong refinement type: %s" (Pprintast.string_of_expression expr))
