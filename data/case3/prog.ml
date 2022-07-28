let abs (x : int) =
  if 0 < x then
    let (y : int) = x + 1 in
    let (z : int) = y - 1 in
    z
  else 0
