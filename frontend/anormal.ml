module NA = Languages.NormalAnormal
module T = Languages.Termlang
module StrucNA = Languages.StrucNA
module Struc = Languages.Struc

let layout to_term code =
  let code = match code.NA.x with NA.Fix (_, body) -> body | _ -> code in
  Expr.layout @@ to_term code

open Sugar
open Zzdatatype.Datatype

let layout_one to_term StrucNA.{ name; body } =
  let open NA in
  let if_rec, body =
    match body.x with Fix (_, body) -> (true, body) | _ -> (false, body)
  in
  let body = to_term body in
  Structure.layout_one Struc.{ if_rec; name; body }

let struct_layout to_term code =
  spf "%s\n" (List.split_by "\n" (layout_one to_term) code)
