let rec reverse (acc : int list) (s : int list) : int list =
  match s with
  | [] -> acc
  | h1 :: tl ->
      let (tmp0 : int list) = h1 :: acc in
      let (tmp1 : int list) = reverse tmp0 tl in
      tmp1
