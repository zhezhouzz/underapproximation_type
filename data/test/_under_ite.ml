let max =
  let x = (v : int) true in
  let y = (v : int) (v < x) in
  (v : int) (v == x)

let max =
  let x = (v : int) (v == 0) in
  let y = (v : int) (v == 1) in
  (v : int) (v == 1)

let max =
  let x = (v : int) true in
  let y = (v : int) true in
  (v : int) (implies (x < y) (v == y) && implies (y < x) (v == x))
