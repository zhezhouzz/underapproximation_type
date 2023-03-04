let[@library] cleaf = (len v 0 && complete v : [%v: int ctree]) [@under]

let[@library] cnode =
  let _ = (true : [%v: int]) [@over] in
  let sizel = (v >= 0 : [%v: int]) [@over] in
  let _ =
    (fun (u : [%forall: int]) -> len v sizel && complete v
      : [%v: int ctree])
      [@under]
  in
  let sizer = (v == sizel : [%v: int]) [@over] in
  let _ =
    (fun (u : [%forall: int]) -> len v sizer && complete v
      : [%v: int ctree])
      [@under]
  in
  (fun (u : [%forall: int]) -> complete v && implies (u == sizel + 1) (len v u)
    : [%v: int ctree])
    [@under]
