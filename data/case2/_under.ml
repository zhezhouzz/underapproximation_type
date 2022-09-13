let neg_relu =
  let x = (v : int) true in
  (v : int) (v == 0)

let neg_relu =
  let x = (v : int) (v > 0) in
  (v : int) (v > 0)

(* Should fail *)
let neg_relu =
  let x = (v : int) (v > 0) in
  (v : int) (v >= 0)
