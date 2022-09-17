(* let foo = *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (fun (u : [%forall: int]) -> implies (mem v u) (mem l u)) *)

let foo =
  let l = (v : int list) true in
  (v : int list) (fun (u : [%forall: int]) -> hd v u && not (mem v u))

(* let foo = *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (fun (u : [%forall: int]) -> false) *)
