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
  let (s : [%ghost: int]) = (true : [%v: int]) in
  let (h : [%over: int]) = (true : [%v: int]) in
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

let[@library] leaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int tree])

let[@library] node =
  let (lo : [%ghost: int]) = (true : [%v: int]) in
  let (hi : [%ghost: int]) = (true : [%v: int]) in
  let (root : [%over: int]) = (lo < v && v < hi : [%v: int]) in
  let (dummy : [%under: int tree]) =
    (fun (u : [%forall: int]) (w : [%forall: int]) ->
       implies (mem v u) (lo < u && u < root) && implies (ord v u w) (u < w)
      : [%v: int tree])
  in
  let (dummy : [%under: int tree]) =
    (fun (u : [%forall: int]) (w : [%forall: int]) ->
       implies (mem v u) (root < u && u < hi) && implies (ord v u w) (u < w)
      : [%v: int tree])
  in
  (fun (u : [%forall: int]) (w : [%forall: int]) ->
     hd v root
     && implies (mem v u) (lo < u && u < hi)
     && implies (ord v u w) (u < w)
    : [%v: int tree])
