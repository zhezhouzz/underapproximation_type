let foo (int_gen : unit -> int) (f : int -> int) (g : int -> int) : int =
  let (x : int) = int_gen () in
  let (y : int) = f x in
  let (z : int) = g x in
  y + z
