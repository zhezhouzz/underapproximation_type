let foo =
  let x = (v : int) true in
  (* let y = (v : int) true in *)
  let a = (v : int list) true in
  (v : int list) (fun (u : [%forall: int]) ->
      implies (mem v u) (u == x) && mem v x)

let foo =
  let x = (v : int) true in
  (* let y = (v : int) true in *)
  let a = (v : int list) true in
  (v : int list) (fun (u : [%forall: int]) ->
      implies (mem v u) (u == x) && not (empty v))

(* let foo (u : 'forall * int) = *)
(*   let x = (v : int) true in *)
(*   let y = (v : int) true in *)
(*   (v : int list) (implies (mem v u) (u = y)) *)

(* let foo (u : 'forall * int) = *)
(*   let x = (v : int) true in *)
(*   let y = (v : int) true in *)
(*   (v : int list) (not (mem v u)) *)
