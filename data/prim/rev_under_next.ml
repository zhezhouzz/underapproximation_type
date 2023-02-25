let[@library] nil = (fun (u : [%forall: int]) -> not (mem v u) : [%v: int list])

let[@library] cons =
  let l =
    (fun (u : [%forall: int]) (w : [%forall: int]) ->
       implies (next v u w) (u <= w)
      : [%v: int list])
  in
  ( t
      (fun (u : [%forall: int]) (w : [%forall: int]) ->
         implies (next v u w) (u <= w)
        : [%v: int list]),
    h (fun (u : [%forall: int]) -> implies (mem t u) (v <= u) : [%v: int]) )
