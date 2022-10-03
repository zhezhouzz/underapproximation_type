let foo =
  let l = (true : [%v: int tree]) in
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int tree])
