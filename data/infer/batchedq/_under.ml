let[@library] rev =
  let (l : [%poly: int list]) = () in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd l u) : [%v: int list])

let tail =
  let f =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) : [%v: int list])
  in
  let r =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd f u) : [%v: int list])
  in
  ( vf (fun (u : [%forall: int]) -> implies (mem v u) (hd f u) : [%v: int list]),
    vr (fun (u : [%forall: int]) -> implies (mem v u) (hd f u) : [%v: int list])
  )
