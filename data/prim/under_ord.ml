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

let[@library] nil = (fun (u : [%forall: int]) -> not (mem v u) : [%v: int list])

let[@library] cons =
  let (h : [%poly: int]) = () in
  let t =
    (fun (u : [%forall: int]) (w : [%forall: int]) ->
       implies (ord v u w) (u <= w) && not (empty v)
      : [%v: int list])
  in
  (fun (u : [%forall: int]) (w : [%forall: int]) ->
     iff (hd v u) (h == u)
     && implies (ord v u w && not (hd v u)) (u <= w)
     && implies (mem t u) (ord v h u)
    : [%v: int list])
