let neg_relu =
  let x = (true : [%v: int]) in
  (v == 0 : [%v: int])

let neg_relu =
  let x = (true : [%v: int]) in
  (v >= 0 : [%v: int])

let neg_relu =
  let x = (v > 0 : [%v: int]) in
  (v > 0 : [%v: int])

(* Should fail *)
(* let neg_relu = *)
(*   let x = (v > 0 : [%v: int]) in *)
(*   (v >= 0 : [%v: int]) *)
