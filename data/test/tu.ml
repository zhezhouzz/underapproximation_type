let rec f (x : int) (y : bool list) (z : int -> int -> int) =
  let (a : int * int), (b : int), (c : int * int) =
    ((y, z), ((x, y), (y, z)), x)
  in
  f b c a
