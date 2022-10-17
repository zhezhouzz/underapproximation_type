let nat_gen (g : unit -> int) : int =
  let (x : int) = g () in
  let (y : int) = g () in
  let (z : int) = x + y in
  let (b : bool) = z < 0 in
  if b then Exn else z
