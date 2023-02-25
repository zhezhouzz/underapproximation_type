let merge (h1_ : int) (h2_ : int) (l1 : int list) (l2 : int list) : int list =
  match l1 with
  | [] -> l2
  | h1 :: t1 -> ( match l2 with [] -> l1 | h2 :: t2 -> Exn)
