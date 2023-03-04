let merge =
  let l1 = (true : [%v: int list]) in
  let l2 = (true : [%v: int list]) in
  (fun (u : [%forall: int]) -> not (ord v u u) : [%v: int list])
