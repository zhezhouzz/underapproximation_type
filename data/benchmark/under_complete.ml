let[@library] leaf = (len v 0 && complete v : [%v: int tree]) [@under]

let[@library] node =
  let _ = (true : [%v: int]) [@over] in
  let sizel = (v >= 0 : [%v: int]) [@over] in
  let _ =
    (fun (u : [%forall: int]) -> len v sizel && complete v
      : [%v: int tree])
      [@under]
  in
  let sizer = (v == sizel : [%v: int]) [@over] in
  let _ =
    (fun (u : [%forall: int]) -> len v sizer && complete v
      : [%v: int tree])
      [@under]
  in
  (fun (u : [%forall: int]) -> complete v && implies (u == sizel + 1) (len v u)
    : [%v: int tree])
    [@under]
