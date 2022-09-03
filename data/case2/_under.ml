let neg_relu =
  let x = (v : int) true in
  (v : int) (v == 0)

let neg_relu (u : 'forall * int) =
  let x = (v : int) (u < v) in
  (v : int) (0 <= v && u < v)

let neg_relu =
  let x = (v : int) (v > 0) in
  (v : int) (v > 0)

(* let neg_relu = *)
(*   let x = (v : int) (v > 0) in *)
(*   (v : int) (v >= 0) *)
