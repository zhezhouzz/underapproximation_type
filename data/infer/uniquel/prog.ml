let rec set_add (a : int) (x : int list) : int list =
  match x with
  | [] ->
      let (s1 : int list) = a :: x in
      s1
  | a1 :: x1 ->
      let (b : bool) = a == a1 in
      if b then
        let (s2 : int list) = a1 :: x1 in
        s2
      else
        let (s3 : int list) = set_add a x1 in
        let (s4 : int list) = a1 :: s3 in
        s4
