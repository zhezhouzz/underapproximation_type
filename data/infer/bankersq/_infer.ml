let[@library] reverse =
  let (l : [%poly: int list]) = () in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd l u) : [%v: int list])

let[@library] concat =
  let (l1 : [%poly: int list]) = () in
  let (l2 : [%poly: int list]) = () in
  (fun (u : [%forall: int]) (w : [%forall: int]) (z : [%forall: int]) ->
     implies (mem v u) (hd l1 u)
    : [%v: int list])

let snoc =
  let lenf = (v >= 0 : [%v: int]) in
  let f =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) : [%v: int list])
  in
  let lenr = (v >= 0 && v <= lenf : [%v: int]) in
  let r =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd f u) : [%v: int list])
  in
  let x = (hd f v : [%v: int]) in
  ( vlenf (v == lenf + lenr + 1 || v == lenf : [%v: int]),
    vf (fun (u : [%forall: int]) -> implies (mem v u) (hd f u) : [%v: int list]),
    vlenr (v + vlenf == lenf + lenr + 1 : [%v: int]),
    (* vr (fun (u : [%forall: int]) -> true : [%v: int list]) *)
    (* vr (fun (u : [%forall: int]) -> not (mem v u) : [%v: int list]) *)
    vr (fun (u : [%forall: int]) -> implies (mem v u) (hd f u) : [%v: int list])
  )
