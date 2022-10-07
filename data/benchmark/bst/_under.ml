let[@library] int_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: int])

let[@inv? n when s] sorted_list_gen =
  let (s : [%over: int]) = (v >= 0 : [%v: int]) in
  let (x : [%over: int]) = (true : [%v: int]) in
  let (n : [%ghost: int]) = (v == s && v >= 0 : [%v: int]) in
  (len v s && fun (u : [%forall: int]) (w : [%forall: int]) ->
   implies (mem v u) (x <= u) && implies (ord v u w) (u <= w)
    : [%v: int list])
