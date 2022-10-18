let[@library] int_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: int])

let[@library] bool_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: bool])

let heap_gen =
  let (s : [%over: int]) = (0 <= v : [%v: int]) in
  let (max : [%over: int]) = (true : [%v: int]) in
  (heap v && len v s && fun (u : [%forall: int]) -> implies (hd v u) (u <= max)
    : [%v: int heap])
