let[@library] makeT =
  let (x : [%poly: int]) = () in
  let h1 =
    (fun (u : [%forall: int]) -> implies (mem v u) (u == x)
      : [%v: int leftisthp])
  in
  let h2 =
    (fun (u : [%forall: int]) -> implies (mem v u) (u == x)
      : [%v: int leftisthp])
  in
  (fun (u : [%forall: int]) -> (not (empty v)) && implies (mem v u) (u == x)
    : [%v: int leftisthp])

let merge =
  let h1 =
    (fun (u : [%forall: int]) -> (not (empty v)) && implies (mem v u) (hd v u)
      : [%v: int leftisthp])
  in
  let h2 =
    (fun (u : [%forall: int]) -> (not (empty v)) && implies (mem v u) (hd h1 u)
      : [%v: int leftisthp])
  in
  (fun (u : [%forall: int]) -> (not (empty v)) && implies (mem v u) (hd h1 u)
    : [%v: int leftisthp])
