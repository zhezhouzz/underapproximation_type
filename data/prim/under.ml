let eq (x : 'forall * int) (y : 'forall * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : bool) (iff v (a == b))

let neq (x : 'forall * int) (y : 'forall * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : bool) (iff v (a != b))

let lt (x : 'forall * int) (y : 'forall * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : bool) (iff v (a < b))

let gt (x : 'forall * int) (y : 'forall * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : bool) (iff v (a > b))

let le (x : 'forall * int) (y : 'forall * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : bool) (iff v (a <= b))

let ge (x : 'forall * int) (y : 'forall * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : bool) (iff v (a => b))

let plus (x : 'forall * int) (y : 'forall * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : int) (v == a + b)

let minus (x : 'forall * int) (y : 'forall * int) =
  let a = (v : int) (v == x) in
  let b = (v : int) (v == y) in
  (v : int) (v == a - b)

let nil (u : 'forall * int) = (v : int list) (not (mem v u))

let cons (u : 'forall * int) =
  let h = (v : int) false in
  let t = (v : int list) (implies (mem v u) (u == h)) in
  (v : int list) (implies (mem v u) (u == h) && mem v h && not (mem v u))

let _ret_two_value =
  let x = (v : int) (v > 0) in
  (v : int) (v == 1 || v == x)
