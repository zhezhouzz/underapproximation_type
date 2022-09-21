let rec rotate (q1 : int list) (q2 : int list) (q3 : int list) : int list =
  match q1 with
  | [] -> (
      match q2 with
      | [] -> Exn
      | y1 :: ys1 ->
          let (r1 : int list) = y1 :: q3 in
          r1)
  | x :: xs -> (
      match q2 with
      | [] -> Exn
      | y2 :: ys2 ->
          let (r2 : int list) = y2 :: q3 in
          let (r3 : int list) = rotate xs ys2 r2 in
          let (r4 : int list) = x :: r3 in
          r4)
