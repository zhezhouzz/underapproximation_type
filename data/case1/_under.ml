let foo =
  let x = (v > 0 : [%v: int]) in
  (v == x + 1 : [%v: int])

let foo =
  let x = (v > 0 : [%v: int]) in
  (v > 1 : [%v: int])

let foo =
  let x = (v > 0 : [%v: int]) in
  (v == 2 : [%v: int])

let foo =
  let x = (true : [%v: int]) in
  (v == 2 : [%v: int])

let foo =
  let x = (true : [%v: int]) in
  (false : [%v: int])

let foo =
  let x = (true : [%v: int]) in
  (true : [%v: int])

let foo =
  let x = (false : [%v: int]) in
  (false : [%v: int])

let foo =
  let x = (v > 0 : [%v: int]) in
  (v == 2 || v == 3 : [%v: int])

(* Should fail *)
let foo =
  let x = (false : [%v: int]) in
  (true : [%v: int])
