let[@library] reverse =
  let (l : [%poly: int list]) = () in
  (fun (u : [%forall: int]) -> iff (len v u) (len l u) : [%v: int list])

let[@library] concat =
  let (l1 : [%poly: int list]) = () in
  let (l2 : [%poly: int list]) = () in
  (fun (u : [%forall: int]) (w : [%forall: int]) (z : [%forall: int]) ->
     iff (len v u && len l1 w && len l2 z) (u == w + z)
    : [%v: int list])

(* let snoc = *)
(*   let lenf = (v > 0 : [%v: int]) in *)
(*   let f = (len v lenf : [%v: int list]) in *)
(*   let lenr = (v >= 0 && v < lenf : [%v: int]) in *)
(*   let r = (len v lenr : [%v: int list]) in *)
(*   let x = (true : [%v: int]) in *)
(*   ( vlenf (v > 0 : [%v: int]), *)
(*     vf (len v vlenf : [%v: int list]), *)
(*     vlenr (v == lenr + 1 : [%v: int]), *)
(*     vr (len v vlenr : [%v: int list]) ) *)

let snoc =
  let lenf = (v > 0 : [%v: int]) in
  let f = (len v lenf : [%v: int list]) in
  let lenr = (v >= 0 : [%v: int]) in
  let r = (len v lenr : [%v: int list]) in
  let x = (true : [%v: int]) in
  ( vlenf (v == lenf + lenr : [%v: int]),
    vf (len v vlenf : [%v: int list]),
    vlenr (v == 0 : [%v: int]),
    vr (len v vlenr : [%v: int list]) )
