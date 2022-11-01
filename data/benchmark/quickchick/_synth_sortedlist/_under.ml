let[@library] gt_eq_int_gen =
  let (x : [%over: int]) = (true : [%v: int]) in
  (v >= x : [%v: int])

let[@library] sizecheck =
  let (x : [%over: int]) = (true : [%v: int]) in
  (iff v (x == 0) : [%v: bool])

let[@library] subs =
    let (s : [%over: int]) = (true : [%v: int]) in
    (v == (s - 1) : [%v: int])
  
  

let goal =
  let (s : [%over: int]) = (v>=0 : [%v: int]) in
  let (x0 : [%over: int]) = (true : [%v: int]) in
  (rng v s && fun (u : [%forall: int]) (w : [%forall: int]) ->
   implies (mem v u) (x0 <= u) && implies (ord v u w) (u <= w)
    : [%v: int list])


