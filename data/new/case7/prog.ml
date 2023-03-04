let foo (x : int) : int =
  if x < 0 then
    let (y : int) = bar x in
    y
  else Exn
