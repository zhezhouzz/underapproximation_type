module T = Ast.Termlang
module StrucNA = Ast.StrucNA
module Struc = Ast.Struc

let layout to_term code =
  let open Ast.NL in
  let code =
    match code.x with
    | V (Fix (_, body)) -> { ty = body.ty; x = V body.x }
    | _ -> code
  in
  Expr.layout @@ to_term code

open Sugar
open Zzdatatype.Datatype

let layout_one to_term StrucNA.{ name; body } =
  let open Ast.NL in
  let if_rec, body =
    match body.x with
    | V (Fix (_, body)) -> (true, { ty = body.ty; x = V body.x })
    | _ -> (false, body)
  in
  let body = to_term body in
  Structure.layout_one Struc.{ if_rec; name; body }

let struct_layout to_term code =
  spf "%s\n" (List.split_by "\n" (layout_one to_term) code)
