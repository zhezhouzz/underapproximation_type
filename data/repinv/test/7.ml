let foo (n : int) : int =
  let (m : int) = n in
  m

let[@assert] foo =
  let n = (v > 0 : [%v: int]) [@under] in
  (v == 1 : [%v: int]) [@under]
