let[@library] int_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: int])

let[@library] sizecheck =
  let (x : [%over: int]) = (true : [%v: int]) in
  (iff v (x == 0) && iff (not v) (x > 0) : [%v: bool])

let[@library] subs =
    let (s : [%over: int]) = (true : [%v: int]) in
    (v == (s - 1) : [%v: int])

let[@library] bool_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: bool])

let[@library] s01 = 
   (true : [%v: int])

let[@library] s = 
   (true : [%v: int])   
let goal =
  let (s : [%over: int]) = (0 <= v : [%v: int]) in
  (fun (u : [%forall: int]) -> implies (len v u) (0 <= u && u <= s)
    : [%v: int tree])
