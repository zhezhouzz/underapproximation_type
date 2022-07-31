let foo (x : int) (l : int list) =
  let (z : int list) = match x :: l with [] -> [] | h :: t -> t in
  x :: z
