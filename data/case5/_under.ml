let identity =
  let x = (v : int) (v > 0) in
  (v : int) (v == 1 || v == x)

let identity =
  let x = (v : int) (v > 0) in
  (v : int) (v > 0 || v == x)

let identity =
  let x = (v : int) (v > 0) in
  (v : int) (v == x + 1)

let identity =
  let x = (v : int) true in
  (v : int) (v == x - 1)

let identity =
  let x = (v : int) (v > 0) in
  (v : int) (v == x - 1)
