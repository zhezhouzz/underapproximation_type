let tail (x : int) (l : int list) : int list =
  let (l' : int list) = x :: l in
  match l' with [] -> [] | h :: t -> t
