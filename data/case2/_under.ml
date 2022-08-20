let abs =
  let x = (v : int) true in
  (v : int) (v == 0)

let abs (u : 'forall * int) =
  let x = (v : int) (u < v) in
  (v : int) (0 <= v && u < v)

let abs (u : 'exists * int) =
  let x = (v : int) (v == u) in
  (v : int) (if x < 0 then v == 0 else v == x)

let abs =
  let x = (v : int) (v > 0) in
  (v : int) (v >= 0)
