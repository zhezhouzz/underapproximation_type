val bar : int -> int

let[@library] bar =
  let n = (v > 0 : [%v: int]) [@under] in
  (v > 0 : [%v: int]) [@under]

let foo (n : int) (m : int) : int =
  let (x : int) = n + 1 in
  let (y : int) = x + m in
  let (z : int) = bar y in
  z

let[@assert] foo =
  let n = (true : [%v: int]) [@under] in
  let m = (true : [%v: int]) [@under] in
  (v > 0 : [%v: int]) [@under]
