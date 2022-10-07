let nat_gen (g : unit -> int) (u : unit) : int =
  let (x : int) = g u in
  let (c : int) = 0 in
  let (b : bool) = x < 0 in
  if b then Exn else x
