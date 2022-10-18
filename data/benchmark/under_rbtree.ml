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
  let (dummy : [%under: int]) = (true : [%v: int]) in
  let (s : [%over: int]) = (v >= 0 : [%v: int]) in
  let (dummy : [%under: int list]) = (len v s : [%v: int list]) in
  (fun (u : [%forall: int]) -> implies (u == s + 1) (len v u) : [%v: int list])

let[@library] batchedq =
  let (s1 : [%over: int]) = (v >= 0 : [%v: int]) in
  let (dummy : [%under: int list]) = (len v s1 : [%v: int list]) in
  let (s2 : [%over: int]) = (v >= 0 : [%v: int]) in
  let (dummy : [%under: int list]) =
    (fun (u : [%forall: int]) -> implies (0 <= u && u <= s1) (len v u)
      : [%v: int list])
  in
  (len v s1 : [%v: int batchedq])

(* color black *)
let[@library] rbtleaf = (numblack v 0 : [%v: int rbtree])

let[@library] rbtnode =
  let (c : [%over: bool]) = (not v : [%v: bool]) in
  let (sizel : [%over: int]) = (v >= 0 : [%v: int]) in
  let (dummy : [%under: int rbtree]) =
    (numblack v sizel && implies (sizel == 0) (hdcolor v true)
      : [%v: int rbtree])
  in
  let (dummy : [%under: int]) = (true : [%v: int]) in
  let (sizer : [%over: int]) = (v == sizel : [%v: int]) in
  let (dummy : [%under: int rbtree]) =
    (numblack v sizer && implies (sizer == 0) (hdcolor v true)
      : [%v: int rbtree])
  in
  (fun (u : [%forall: int]) ->
     hdcolor v false && implies (u == sizel + 1) (numblack v u)
    : [%v: int rbtree])

(* color red *)
let[@library] rbtnode =
  let (c : [%over: bool]) = (v : [%v: bool]) in
  let (sizel : [%over: int]) = (v >= 0 : [%v: int]) in
  let (dummy : [%under: int rbtree]) =
    (numblack v sizel && hdcolor v false : [%v: int rbtree])
  in
  let (dummy : [%under: int]) = (true : [%v: int]) in
  let (sizer : [%over: int]) = (v == sizel : [%v: int]) in
  let (dummy : [%under: int rbtree]) =
    (numblack v sizer && hdcolor v false : [%v: int rbtree])
  in
  (hdcolor v true && numblack v sizel : [%v: int rbtree])
