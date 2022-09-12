let tail =
  let x = (v : int) true in
  let l = (v : int list) true in
  (v : int list) (fun (u : [%forall: int]) -> implies (mem v u) (u == x))

let tail =
  let x = (v : int) true in
  let l = (v : int list) true in
  (v : int list) (fun (u : [%forall: int]) -> not (mem v u))

(* Should fail *)

let tail =
  let x = (v : int) true in
  let l = (v : int list) true in
  (v : int list) (mem v x)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (not (mem v x)) *)
