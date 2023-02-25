let merge =
  let h1 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u)
      : [%v: int pairinghp])
  in
  let h2 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd h1 u)
      : [%v: int pairinghp])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd h1 u)
    : [%v: int pairinghp])
