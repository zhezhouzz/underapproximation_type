let rec merge (s1 : int list) (s2 : int list) : int list =
  match s1 with
  | [] -> s2
  | h1 :: t1 -> (
      match s2 with
      | [] -> s1
      | h2 :: t2 ->
          let (s3 : int list) = merge t1 t2 in
          if h1 < h2 then h1 :: h2 :: s3
          else if h1 > h2 then h2 :: h2 :: s3
          else h1 :: s3)
