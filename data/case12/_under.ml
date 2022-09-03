let foo =
  let x = (v : int) (v == 1) in
  (v : int) (v == x + x)

let foo =
  let x = (v : int) true in
  (v : int) (v == x + x && x == 1)

let foo =
  let x = (v : int) false in
  (v : int) (v == x)

let foo =
  let x = (v : int) (v > 0) in
  (v : int) (v > 1)

let foo =
  let x = (v : int) (v > 0) in
  (v : int) (v == x)

(* let foo = *)
(*   let x = (v : int) true in *)
(*   (v : int) (v == x + x) *)

(* let foo = *)
(*   let x = (v : int) true in *)
(*   (v : int) (v == x) *)
