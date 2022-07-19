let rec f (x : int) (l : int list) : int =
  match x :: l with [] -> x | h :: t -> f h t
