let nat_gen (g : unit -> int) (u : unit) : int =
  let (x : int) = g u in
  let (y : int) = g u in
  let (z : int) = x + y in
  let (b : bool) = z < 0 in
  if b then Exn else z
