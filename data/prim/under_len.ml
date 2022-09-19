let[@library] eq =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (iff v (a == b) : [%v: bool])

let[@library] neq =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (iff v (a != b) : [%v: bool])

let[@library] lt =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (iff v (a < b) : [%v: bool])

let[@library] gt =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (iff v (a > b) : [%v: bool])

let[@library] le =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (iff v (a <= b) : [%v: bool])

let[@library] ge =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (iff v (a => b) : [%v: bool])

let[@library] plus =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (v == a + b : [%v: int])

let[@library] minus =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (v == a - b : [%v: int])

let[@library] nil = (len v 0 : [%v: int list])

let[@library] cons =
  let (h : [%poly: int]) = () in
  let (t : [%poly: int list]) = () in
  (fun (u : [%forall: int]) (w : [%forall: int]) ->
     iff (u == w + 1) (len t w && len v u)
    : [%v: int list])
