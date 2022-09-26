let[@library] nil = (fun (u : [%forall: int]) -> not (mem v u) : [%v: int list])

let[@library] cons =
  let l =
    (fun (u : [%forall: int]) (w : [%forall: int]) ->
       implies (ord v u w) (u <= w) && not (empty v)
      : [%v: int list])
  in
  ( h (fun (u : [%forall: int]) -> iff (hd l u) (v == u) : [%v: int]),
    t
      (fun (u : [%forall: int]) (w : [%forall: int]) ->
         implies (ord v u w) (u <= w)
        : [%v: int list]) )
