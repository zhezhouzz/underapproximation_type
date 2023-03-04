let foo (x : int) (y : int) (z : int) : int =
  let (c1 : int) = 1 in
  let (a1 : int) = x + c1 in
  let (a2 : int) = a1 + z in
  a2
