let[@library] int_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: int])

let[@inv? n when 0] list_gen =
  let (n : [%ghost: int]) = (v >= 0 : [%v: int]) in
  let (s : [%over: int]) = (v >= 0 && v == n : [%v: int]) in
  (len v s : [%v: int list])

let[@inv? n when 0] list_gen =
  let (n : [%ghost: int]) = (v >= 1 : [%v: int]) in
  let (s : [%over: int]) = (v >= 0 && v == n : [%v: int]) in
  (len v s : [%v: int list])
