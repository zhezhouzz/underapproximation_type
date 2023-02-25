let[@library] ret_two_value =
  let x = (v > 0 : [%v: int]) in
  (v == 1 || v == 2 : [%v: int])

let foo =
  let x = (v >= 0 : [%v: int]) in
  (v == 3 : [%v: int])
