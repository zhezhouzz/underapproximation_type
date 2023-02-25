let foo =
  let x = (true : [%v: int]) in
  let y = (v < x : [%v: int]) in
  let z = (v > 0 : [%v: int]) in
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int list])
