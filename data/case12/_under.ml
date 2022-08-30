let foo =
  let x = (v : int) (v > 0) in
  (v : int) (v == x + x)

(* let foo = *)
(*   let x (a : int) = (v : int) (v == a) in *)
(*   (v : int) (v == x + x) *)

let foo =
  let x = (v : int) true in
  (v : int) (v == x)
