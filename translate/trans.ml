module S = Languages.Struc
module SN = Languages.StrucNA
module N = Languages.NormalAnormal
module T = Languages.Termlang
open Sugar

let term_to_nan code = Na.simplify @@ Term2normalanormal.to_anormal code
let nan_to_term = Term2normalanormal.to_term

let to_anormal_with_name x if_rec (e : T.term T.opttyped) : N.term N.typed =
  let open N in
  let e = Na.simplify @@ Term2normalanormal.to_anormal e in
  if if_rec then
    match e.x with
    | V v -> { ty = e.ty; x = V (Fix ({ ty = e.ty; x }, { ty = e.ty; x = v })) }
    | LetApp { ret; _ } ->
        let () = Printf.printf "%s\n" ret.x in
        _failatwith __FILE__ __LINE__ ""
    | LetVal { lhs; body = { ty = _; x = V (Var id) }; _ } ->
        let () = Printf.printf "let %s = _ in %s \n" lhs.x id in
        _failatwith __FILE__ __LINE__ ""
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
      | N.(V (Fix (_, body))) ->
          let body = Term2normalanormal.to_term @@ N.value_to_term body in
          S.{ name; if_rec = true; body }
      | _ -> S.{ name; if_rec = false; body = Term2normalanormal.to_term body })
    code
