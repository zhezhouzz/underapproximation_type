let foo =
  let x = (true : [%v: int]) in
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int list])

let foo =
  let x = (true : [%v: int]) in

  (fun (u : [%forall: int]) (w : [%forall: int]) -> not (mem v u)
    : [%v: int list])

let foo =
  let x = (true : [%v: int]) in

  (fun (u : [%forall: int]) (w : [%forall: int]) ->
     (not (mem v u)) && not (mem v w)
    : [%v: int list])

(* should fail *)
let foo =
  let x = (true : [%v: int]) in
  (not (mem v x) : [%v: int list])
