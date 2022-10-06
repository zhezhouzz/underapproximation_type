let[@library] bar =
  let x = (v == 0 : [%v: int]) in
  (v == 1 : [%v: int])

let foo =
  let x = (v >= 0 : [%v: int]) in
  (v == 2 : [%v: int])

let foo =
  let x = (false : [%v: int]) in
  (v == 2 : [%v: int])
