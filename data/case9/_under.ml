let foo (u : 'forall * int) (w : 'exists * int) =
  let l = (v : int_tree) true in
  (v : int_tree) (implies (mem v u) (u == w))
