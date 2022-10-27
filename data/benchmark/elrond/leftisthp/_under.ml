let[@library] int_range =
  let (x : [%over: int]) = (true : [%v: int]) in
  let (y : [%over: int]) = (true : [%v: int]) in
  (x <= v && v <= y : [%v: int])

let[@library] int_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: int])

let[@library] bool_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: bool])

let leftisthp_gen =
  let (s : [%over: int]) = (0 <= v : [%v: int]) in
  (len v s : [%v: int leftisthp])
