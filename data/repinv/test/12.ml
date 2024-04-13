val bar : int -> int

let[@library] bar =
  let n = (v > 0 : [%v: int]) [@under] in
  (v > 0 : [%v: int]) [@under]

let foo (n : bool) : bool =
  let (x : bool) = n in
  if n then
    let (y : bool) = x in
    true
  else false

let[@assert] foo =
  let n = (true : [%v: bool]) [@under] in
  (v : [%v: bool]) [@under]
