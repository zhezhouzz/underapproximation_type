let rec f x y z =
  let a, b, c = ((y, z), ((x, y), (y, z)), x) in
  f b c a
