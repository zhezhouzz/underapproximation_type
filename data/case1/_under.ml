let foo =
  let x = (v : int) (v > 0) in
  (v : int) (v == x + 1)

let foo =
  let x = (v : int) (v > 0) in
  (v : int) (v > 1)

let foo =
  let x = (v : int) (v > 0) in
  (v : int) (v == 2)

let foo =
  let x = (v : int) true in
  (v : int) (v == 2)

let foo =
  let x = (v : int) true in
  (v : int) false

let foo =
  let x = (v : int) true in
  (v : int) true

let foo =
  let x = (v : int) false in
  (v : int) false

let foo =
  let x = (v : int) (v > 0) in
  (v : int) (v == 2 || v == 3)

(* let foo = *)
(*   let x = (v : int) false in *)
(*   (v : int) true *)

(* Should fail *)
