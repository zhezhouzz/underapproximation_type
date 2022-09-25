let[@library] rev =
  let l =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) : [%v: int list])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd l u) : [%v: int list])

let[@library] append =
  let (l1 : [%poly: int list]) = () in
  let (l2 : [%poly: int list]) = () in
  (fun (u : [%forall: int]) (w : [%forall: int]) (z : [%forall: int]) ->
     implies (mem v u) (hd l1 u)
    : [%v: int list])

let check =
  let l =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) : [%v: int list])
  in
  let lenf = (true : [%v: int]) in
  let f =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd l u) : [%v: int list])
  in
  let lenr = (true : [%v: int]) in
  let r =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd l u) : [%v: int list])
  in
  ( vl (fun (u : [%forall: int]) -> implies (mem v u) (hd l u) : [%v: int list]),
    vlenf (true : [%v: int]),
    vf (fun (u : [%forall: int]) -> implies (mem v u) (hd l u) : [%v: int list]),
    vlenr (true : [%v: int]),
    vr (fun (u : [%forall: int]) -> implies (mem v u) (hd l u) : [%v: int list])
  )
