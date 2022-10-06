let[@inv? n when len] list_gen =
  let (len : [%poly: int]) = () in
  let (x : [%poly: int]) = () in
  (fun (u : [%forall: int]) -> len v len && implies (mem v u) (u == x)
    : [%v: int list])
