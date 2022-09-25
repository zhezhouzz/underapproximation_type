let set_add =
  let a = (true : [%v: int]) in
  let x =
    (fun (u : [%forall: int]) -> (not (empty v)) && implies (mem v u) (hd v u)
      : [%v: int list])
  in
  (fun (u : [%forall: int]) -> (not (empty v)) && implies (mem v u) (hd v u)
    : [%v: int list])
