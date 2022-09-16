let foo =
  let l = (v : int_tree) true in
  (v : int_tree) (fun (u : [%forall: int]) -> not (mem v u))
