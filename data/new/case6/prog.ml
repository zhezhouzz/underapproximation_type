let nat_gen (app : (unit -> int) -> int) (g : unit -> int) : int =
  let (x : int) = app g in
  let (y : int) = app g in
  let (z : int) = x + y in
  let (b : bool) = z < 0 in
  if b then Exn else z
