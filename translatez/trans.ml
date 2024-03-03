open Ast
module S = Struc
module SN = StrucNA
module N = NL
module T = Termlang
open NNtyped
open Sugar

(* let term_to_nan code = Na.simplify @@ Term2normalanormal.to_anormal code *)
let nan_to_term = Term2normalanormal.to_term

let to_anormal_with_name x if_rec (e : T.term T.opttyped) : N.term typed =
  let open N in
  let e = Na.simplify @@ Term2normalanormal.to_anormal e (Some x) in
  if if_rec then
    let v = N.term_to_value __FILE__ __LINE__ e in
    match v.x with
    | Lam { lamarg; lambody } ->
        {
          x =
            V
              {
                x = Fix { fixname = { ty = e.ty; x }; fstarg = lamarg; lambody };
                ty = e.ty;
              };
          ty = v.ty;
        }
    | _ -> _failatwith __FILE__ __LINE__ ""
  else e

let struc_term_to_nan code =
  List.map
    (fun S.{ name; if_rec; body } ->
      let body = to_anormal_with_name name if_rec body in
      SN.{ name; body })
    code

let struc_nan_to_term code =
  List.map
    (fun SN.{ name; body } ->
      match body.x with
      | N.(V { x = Fix { fstarg; lambody; _ }; ty }) ->
          let body =
            Term2normalanormal.to_term
            @@ N.value_to_term { x = Lam { lamarg = fstarg; lambody }; ty }
          in
          S.{ name; if_rec = true; body }
      | _ -> S.{ name; if_rec = false; body = Term2normalanormal.to_term body })
    code
