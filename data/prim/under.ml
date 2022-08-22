let eq (x : 'exists * int) (y : 'exists * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : bool) (iff v (a == b))

let neq (x : 'exists * int) (y : 'exists * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : bool) (iff v (a != b))

let lt (x : 'exists * int) (y : 'exists * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : bool) (iff v (a < b))

let gt (x : 'exists * int) (y : 'exists * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : bool) (iff v (a > b))

let le (x : 'exists * int) (y : 'exists * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : bool) (iff v (a <= b))

let ge (x : 'exists * int) (y : 'exists * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : bool) (iff v (a => b))

let plus (x : 'exists * int) (y : 'exists * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : int) (v == a + b)

let minus (x : 'exists * int) (y : 'exists * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : int) (v == a - b)

let nil (u : 'forall * int) = (v : int list) (not (mem v u))

let cons (u : 'forall * int) (w : 'exists * int) =
  let h = (v : int) (v == w) in
  let t = (v : int list) (implies (mem v u) (u == h)) in
  (v : int list) (implies (mem v u) (u == h) && mem v h)

let _ret_two_value =
  let x = (v : int) (v > 0) in
  (v : int) (v == 1 || v == x)
