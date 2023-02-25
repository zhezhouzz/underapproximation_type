let[@library] int_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: int])

let list_gen =
  let (s : [%over: int]) = (v >= 0 : [%v: int]) in
  (len v s : [%v: int list])

let list_gen =
  let (s : [%over: int]) = (v >= 0 : [%v: int]) in
  (len v s : [%v: int list])
