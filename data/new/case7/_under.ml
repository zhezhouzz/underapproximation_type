let[@library] bar =
  let (dummy : [%under: int]) = (v < 0 : [%v: int]) in
  (v > 0 : [%v: int])
(* even  *)

let foo =
  let (dummy : [%under: int]) = (true : [%v: int]) in
  (v > 0 : [%v: int])

(* Should fail *)
let foo =
  let (dummy : [%under: int]) = (v < -3 : [%v: int]) in
  (v > 0 : [%v: int])
