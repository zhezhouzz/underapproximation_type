let concat =
  let x = (v : int) true in
  let l1 =
    (v : int list) (fun (u : [%forall: int]) -> implies (mem v u) (u == x))
  in
  let l2 =
    (v : int list) (fun (u : [%forall: int]) -> implies (mem v u) (u == x))
  in
  (v : int list) (fun (u : [%forall: int]) -> true)
