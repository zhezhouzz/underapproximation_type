let concat (u : 'forall * int) (w : 'forall * int) (z : 'exists * int)
    (n : 'exists * int) =
  let l1 = (v : int list) (implies (ord v u w) (u < w)) in
  let l2 = (v : int list) (implies (ord v u w) (u < w)) in
  (v : int list)
    (implies (hd l1 z && hd l2 n && not (z < n)) (ord v z n)
    && implies (empty l1)
         (iff (ord v u w) (ord l2 u w) && iff (hd v u) (hd l2 u)))
