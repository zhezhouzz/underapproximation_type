let foo =
  let x = (v : int) true in
  (v : int list) (fun (u : [%forall: int]) -> not (mem v u))

let foo =
  let x = (v : int) true in
  (v : int list) (fun (u : [%forall: int]) (w : [%forall: int]) ->
      not (mem v u))

let foo =
  let x = (v : int) true in
  (v : int list) (fun (u : [%forall: int]) (w : [%forall: int]) ->
      (not (mem v u)) && not (mem v w))

(* should fail *)
let foo =
  let x = (v : int) true in
  (v : int list) (not (mem v x))
