let rec concat (x : int) (s1 : int list) (s2 : int list) : int list =
  match s1 with
  | [] -> s2
  | h1 :: t1 ->
      let (s3 : int list) = concat x t1 s2 in
      let (s4 : int list) = h1 :: s3 in
      s4
