let identity =
  let x = (v > 0 : [%v: int]) in
  (v == 1 || v == x : [%v: int])

let identity =
  let x = (v > 0 : [%v: int]) in
  (v > 0 || v == x : [%v: int])

let identity =
  let x = (v > 0 : [%v: int]) in
  (v == x + 1 : [%v: int])

let identity =
  let x = (true : [%v: int]) in
  (v == x - 1 : [%v: int])

(* Should fail *)
let identity =
  let x = (v > 0 : [%v: int]) in
  (v == x - 1 : [%v: int])
