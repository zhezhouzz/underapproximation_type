let rec concat (s1 : int list) (s2 : int list) : int list =
  match s1 with [] -> s2 | h1 :: t1 -> h1 :: concat s1 t1
