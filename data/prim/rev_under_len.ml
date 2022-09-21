let[@library] nil = (len v 0 : [%v: int list])

let[@library] cons =
  let l = (not (empty v) : [%v: int list]) in
  ( h (fun (u : [%forall: int]) -> true : [%v: int]),
    t
      (fun (u : [%forall: int]) (w : [%forall: int]) ->
         implies (len v u && lem l w) (u == w + 1)
        : [%v: int list]) )
