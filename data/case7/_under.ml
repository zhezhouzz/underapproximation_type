let foo (u : 'forall * int) (w : 'forall * int) =
  let l = (v : int list) true in
  (v : int list)
    (implies (mem v u) (mem l u) && implies (mem l u && mem l w) (u == w))

let foo (u : 'forall * int) (z : 'exists * int) =
  let l = (v : int list) true in
  (v : int list) (implies (mem v u) (u == z))