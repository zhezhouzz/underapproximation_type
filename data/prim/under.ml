let[@notation] eq =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : bool) (iff v (a == b))

let[@notation] neq =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : bool) (iff v (a != b))

let[@notation] lt =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : bool) (iff v (a < b))

let[@notation] gt =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : bool) (iff v (a > b))

let[@notation] le =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : bool) (iff v (a <= b))

let[@notation] ge =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : bool) (iff v (a => b))

let[@notation] plus =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : int) (v == a + b)

let[@notation] minus =
  let a (x : int) = (v : int) (v == x) in
  let b (y : int) = (v : int) (v == y) in
  (v : int) (v == a - b)

let[@notation] nil (u : 'forall * int) = (v : int list) (not (mem v u))

let[@notation] cons (u : 'forall * int) =
  let h (w : int) = (v : int) (v == w) in
  let t = (v : int list) (implies (mem v u) (u == h)) in
  (v : int list) (implies (mem v u) (u == h) && mem v h)

let[@notation] ileaf (u : 'forall * int) = (v : int_tree) (not (mem v u))

let[@notation] _ret_two_value =
  let x = (v : int) (v > 0) in
  (v : int) (v == 1 || v == x)
