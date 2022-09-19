let foo =
  let l = (v : int list) true in
  (v : int list) (fun (u : [%forall: int]) (w : [%forall: int]) ->
      implies (mem v u) (mem l u) && implies (mem l u && mem l w) (u == w))
