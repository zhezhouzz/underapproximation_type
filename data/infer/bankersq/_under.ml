let[@library] reverse =
  let (l : [%poly: int list]) = () in
  (fun (u : [%forall: int]) -> iff (len v u) (len l u) : [%v: int list])

let[@library] concat =
  let (l1 : [%poly: int list]) = () in
  let (l2 : [%poly: int list]) = () in
  (fun (u : [%forall: int]) (w : [%forall: int]) (z : [%forall: int]) ->
     iff (len v u && len l1 w && len l2 z) (u == w + z)
    : [%v: int list])

let snoc =
  let lenf = (true : [%v: int]) in
  let f = (len v lenf : [%v: int list]) in
  let lenr = (v <= lenf : [%v: int]) in
  let r = (len v lenr : [%v: int list]) in
  let x = (true : [%v: int]) in
  ( vlenf (true : [%v: int]),
    vf (len v vlenf : [%v: int list]),
    vlenr (v <= vlenf : [%v: int]),
    vr (len v vlenr : [%v: int list]) )
