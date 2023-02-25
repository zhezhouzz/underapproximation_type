let[@library] eq =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (iff v (a == b) : [%v: bool])

let[@library] neq =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (iff v (a != b) : [%v: bool])

let[@library] lt =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (iff v (a < b) : [%v: bool])

let[@library] gt =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (iff v (a > b) : [%v: bool])

let[@library] le =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (iff v (a <= b) : [%v: bool])

let[@library] ge =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (iff v (a >= b) : [%v: bool])

let[@library] plus =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (v == a + b : [%v: int])

let[@library] minus =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (v == a - b : [%v: int])

let[@library] tt = (true : [%v: unit])
let[@library] nil = (len v 0 : [%v: int list])

let[@library] cons =
  let (h : [%over: int]) = (true : [%v: int]) in
  let (s : [%over: int]) = (true : [%v: int]) in
  let (dummy : [%under: int list]) =
    (len v s && fun (u : [%forall: int]) (w : [%forall: int]) ->
     implies (mem v u) (h <= u) && implies (ord v u w) (u <= w)
      : [%v: int list])
  in
  (fun (u : [%forall: int]) (w : [%forall: int]) ->
     implies (u == s + 1) (len v u)
     && implies (mem v u) (h <= u)
     && implies (ord v u w) (u <= w)
    : [%v: int list])

let[@library] leaf = (len v 0 : [%v: int tree])

let[@library] node =
  let (root : [%over: int]) = (true : [%v: int]) in
  let (sizel : [%over: int]) = (v >= 0 : [%v: int]) in
  let (dummy : [%under: int tree]) =
    (fun (u : [%forall: int]) ->
       implies (mem v u) (root - sizel < u && u < root)
       && sorted v && len v sizel
      : [%v: int tree])
  in
  let (sizer : [%over: int]) = (v >= 0 : [%v: int]) in
  let (dummy : [%under: int tree]) =
    (fun (u : [%forall: int]) ->
       implies (mem v u) (root < u && u < root + sizer)
       && sorted v && len v sizer
      : [%v: int tree])
  in
  (fun (u : [%forall: int]) ->
     implies (mem v u) (root - sizel < u && u < root + sizer) && sorted v
    : [%v: int tree])
