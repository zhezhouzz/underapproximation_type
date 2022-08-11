let nil (u : 'forall * int) = (v : int list) (not (mem v u))

let cons (u : 'forall * int) (w : 'forall * int) (z : 'exists * int) =
  let l = (v : int list) (implies (mem v u && mem v w) (u == w) && mem v z) in
  ((h : int) (mem l h), (t : int list) (implies (mem t u) (mem l u)))
