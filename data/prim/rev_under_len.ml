let[@library] nil = (mem v 0 : [%v: int list])

let[@library] cons =
  let l = (not (empty v) : [%v: int list]) in
  ( h (fun (u : [%forall: int]) -> iff (hd l u) (v == u) : [%v: int]),
    t
      (fun (u : [%forall: int]) (w : [%forall: int]) ->
         iff (u == w + 1) (len v u && lem l w)
        : [%v: int list]) )
