let eq =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : bool) (iff v (a == b))

let neq =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : bool) (iff v (a != b))

let lt =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : bool) (iff v (a < b))

let gt =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : bool) (iff v (a > b))

let le =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : bool) (iff v (a <= b))

let ge =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : bool) (iff v (a => b))

let plus =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : int) (v == a + b)

let minus =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : int) (v == a - b)

let nil (u : 'forall * int) = (v : int list) (not (mem v u))

let cons (u : 'forall * int) =
  let h (w : int) = (v : int) (v == w) in
  let t = (v : int list) (implies (mem v u) (u == h)) in
  (v : int list) (implies (mem v u) (u == h) && mem v h)

let ileaf (u : 'forall * int) = (v : int_tree) (not (mem v u))

let _ret_two_value =
  let x = (v : int) (v > 0) in
  (v : int) (v == 1 || v == x)
