let[@library] lower_bound_int_gen =
  let (x : [%over: int]) = (true : [%v: int]) in
  (v >= x : [%v: int])

let sorted_list_gen =
  let (s : [%over: int]) = (0 <= v : [%v: int]) in
  let (x : [%over: int]) = (true : [%v: int]) in
  (rng v s && fun (u : [%forall: int]) (w : [%forall: int]) ->
   implies (mem v u) (x <= u) && implies (ord v u w) (u <= w)
    : [%v: int list])
