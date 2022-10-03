let foo (x : int) : int =
  let (f : int -> int) = fun (y : [%t_y: int]) -> y + 1 in
  let (z : int) = f x in
  z
