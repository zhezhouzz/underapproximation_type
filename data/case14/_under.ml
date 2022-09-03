let foo (u : 'forall * int) =
  let x = (v : int) true in
  let y = (v : int) (v < x) in
  let z = (v : int) (v > 0) in
  (v : int list) (not (mem v u))

let foo (u : 'forall * int) (w : 'forall * int) =
  let x = (v : int) (w >= v || w < v) in
  let y = (v : int) (v < x) in
  let z = (v : int) (v > 0) in
  (v : int list) (not (mem v u))
