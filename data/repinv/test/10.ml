val bar : int -> int

let[@library] bar =
  let n = (v > 0 : [%v: int]) [@under] in
  (v >= 0 : [%v: int]) [@under]

let foo (n : int) : int =
  let (m : int) = n in
  bar n

let[@assert] foo =
  let n = (v > 0 : [%v: int]) [@under] in
  (v == 1 : [%v: int]) [@under]
