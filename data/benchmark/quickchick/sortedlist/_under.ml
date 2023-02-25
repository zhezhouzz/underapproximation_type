let[@library] int_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: int])

let sorted_list_gen =
  let (s : [%over: int]) = (0 <= v : [%v: int]) in
  let (x : [%over: int]) = (true : [%v: int]) in
  (rng v s && fun (u : [%forall: int]) (w : [%forall: int]) ->
   implies (mem v u) (x <= u) && implies (ord v u w) (u <= w)
    : [%v: int list])
