let foo =
  let x = (true : [%v: int]) in
  (* let y =  true in *)
  let a = (true : [%v: int list]) in
  (fun (u : [%forall: int]) -> implies (mem v u) (u == x) && mem v x
    : [%v: int list])

let foo =
  let x = (true : [%v: int]) in
  (* let y =  true in *)
  let a = (true : [%v: int list]) in
  (fun (u : [%forall: int]) -> implies (mem v u) (u == x) && not (empty v)
    : [%v: int list])

(* let foo (u : 'forall * int) = *)
(*   let x =  true in *)
(*   let y =  true in *)
(*    (implies (mem v u) (u = y)) *)

(* let foo (u : 'forall * int) = *)
(*   let x =  true in *)
(*   let y =  true in *)
(*    (not (mem v u)) *)
