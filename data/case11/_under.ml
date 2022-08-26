let merge (u : 'forall * int) (w : 'exists * int) (z : 'exists * int)
    (n : 'exists * int) =
  let l1 = (v : int list) (sorted l1) in
  let l2 = (v : int list) (sorted l2) in
  (v : int list)
    (implies
       (hd l1 w && hd l2 z && w < z && ord l1 w n && n > z)
       (ord v n z && n)
       iff ())
