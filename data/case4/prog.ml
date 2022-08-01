let foo (x : int) =
  let (b : bool) = x == 0 in
  if b then 0
  else
    let (y : int) = _ret_two_value x in
    let (z : int) = y + 1 in
    z
