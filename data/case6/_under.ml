let foo (u : 'forall * int) =
  let x = (v : int) true in
  let l = (v : int list) true in
  (v : int list) (implies (mem v u) (u == x))

let foo (u : 'forall * int) =
  let x = (v : int) true in
  let l = (v : int list) true in
  (v : int list) (not (mem v u))
