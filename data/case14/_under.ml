let foo =
  let x = (v : int) true in
  let y = (v : int) (v < x) in
  let z = (v : int) (v > 0) in
  (v : int list) (fun (u : [%forall: int]) -> not (mem v u))
