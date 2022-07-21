let foo (x : int) (l : int list) =
  let (l' : int list) = x :: l in
  match l' with [] -> l | h :: t -> t
