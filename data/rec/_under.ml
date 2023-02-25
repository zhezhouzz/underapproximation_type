let concat =
  let l1 = (fun (u : [%forall: int]) -> not (ord v u u) : [%v: int list]) in
  let l2 = (fun (u : [%forall: int]) -> not (ord v u u) : [%v: int list]) in
  (fun (u : [%forall: int]) -> implies (not (hd v u)) (not (ord v u u))
    : [%v: int list])
