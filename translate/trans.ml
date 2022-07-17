module S = Languages.Struc
module SN = Languages.StrucNA
module N = Languages.NormalAnormal
module T = Languages.Termlang

let term_to_nan code = Na.simplify @@ Term2normalanormal.to_anormal code
let nan_to_term = Term2normalanormal.to_term

let struc_term_to_nan code =
  List.map
    (fun S.{ name; if_rec; body } ->
      let body =
        T.(
          make_untyped
          @@ Let (if_rec, [ make_untyped name ], body, make_untyped_var name))
      in
      let body = Term2normalanormal.to_anormal body in
      SN.{ name; body = Na.simplify body })
    code
