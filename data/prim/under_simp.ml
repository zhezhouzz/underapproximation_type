let[@notation] eq =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a == b))

let[@notation] neq =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a != b))

let[@notation] lt =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a < b))

let[@notation] gt =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a > b))

let[@notation] le =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a <= b))

let[@notation] ge =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a => b))

let[@notation] plus =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : int) (v == a + b)

let[@notation] minus =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : int) (v == a - b)

let[@notation] nil = (v : int list) (empty v)

let[@notation] cons (u : [%forall: int]) =
  let h = (v : int) true in
  let t = (v : int list) (empty v) in
  (v : int list) (implies (mem v u) (hd v u) && hd v h)

let[@notation] ileaf (u : [%forall: int]) = (v : int_tree) (not (mem v u))

let[@notation] _ret_two_value =
  let x = (v : int) (v > 0) in
  (v : int) (v == 1 || v == x)
