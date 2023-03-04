let foo =
  let x = (v == 1 : [%v: int]) in
  (v == x + x : [%v: int])

let foo =
  let x = (true : [%v: int]) in
  (v == x + x && x == 1 : [%v: int])

let foo =
  let x = (v > 0 : [%v: int]) in
  (v > 1 : [%v: int])

let foo =
  let x = (true : [%v: int]) in
  (v == x : [%v: int])

(* should fail *)
let foo =
  let x = (v > 0 : [%v: int]) in
  (v == x : [%v: int])

(* let foo = *)
(*   let x = (false : [%v: int]) in *)
(*   (v == x : [%v: int]) *)

(* let foo = *)
(*   let x =  true in *)
(*    (v == x + x) *)

(* let foo = *)
(*   let x =  true in *)
(*    (v == x) *)
