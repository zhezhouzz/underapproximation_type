val bar : int -> int

let[@library] bar =
  let n = (v > 0 : [%v: int]) [@under] in
  (v > 0 : [%v: int]) [@under]

let foo (n : int) : int =
  let (y : int) = bar n in
  0

let[@assert] foo =
  let n = (true : [%v: int]) [@under] in
  (v == 0 : [%v: int]) [@under]
