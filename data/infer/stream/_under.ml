let reverse =
  let l1 =
    (fun (u : [%forall: int]) -> (not (empty v)) && implies (mem v u) (hd v u)
      : [%v: int list])
  in
  let l2 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd l1 u) : [%v: int list])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd l1 u) : [%v: int list])
