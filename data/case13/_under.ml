let foo =
  let x = (v : int) (v >= 0) in
  let y = (v : int) (v >= 0) in
  let z = (v : int) (v >= 0) in
  (v : int) (v >= 1)

let foo =
  let x = (v : int) (v >= 0) in
  let y = (v : int) (v >= 0) in
  let z = (v : int) (v >= 0) in
  (v : int) (v >= 1 + y)

let foo =
  let x = (v : int) (v >= 0) in
  let y = (v : int) (v >= x) in
  let z = (v : int) (v >= 0) in
  (v : int) (v >= 1 + y)

let foo =
  let x = (v : int) (v >= 0) in
  let y = (v : int) (v >= x) in
  let z = (v : int) (v >= 0) in
  (v : int) (v >= 1 + y + x)
