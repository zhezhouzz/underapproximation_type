let insert =
  let x = (true : [%v: int]) in
  let s =
    (fun (u : [%forall: int]) -> implies (mem v u) (u == x) : [%v: int unbset])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (u == x) && not (empty v)
    : [%v: int unbset])
