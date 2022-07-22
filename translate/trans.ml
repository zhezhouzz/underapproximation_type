module S = Languages.Struc
module SN = Languages.StrucNA
module N = Languages.NormalAnormal
module T = Languages.Termlang

let term_to_nan code = Na.simplify @@ Term2normalanormal.to_anormal code
let nan_to_term = Term2normalanormal.to_term

let struc_term_to_nan code =
  List.map
    (fun S.{ name; if_rec; body } ->
      let body = Term2normalanormal.to_anormal_with_name name if_rec body in
      SN.{ name; body = Na.simplify body })
    code

let struc_nan_to_term code =
  List.map
    (fun SN.{ name; body } ->
      (* TODO: handle rec *)
      let body = Term2normalanormal.to_term body in
      S.{ name; if_rec = false; body })
    code
