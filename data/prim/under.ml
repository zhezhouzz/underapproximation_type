let eq =
  let a = (v : int) false in
  let b = (v : int) false in
  (v : bool) (iff v (a == b))

let neq =
  let a = (v : int) false in
  let b = (v : int) false in
  (v : bool) (iff v (a != b))

let lt =
  let a = (v : int) false in
  let b = (v : int) false in
  (v : bool) (iff v (a < b))

let gt =
  let a = (v : int) false in
  let b = (v : int) false in
  (v : bool) (iff v (a > b))

let le =
  let a = (v : int) false in
  let b = (v : int) false in
  (v : bool) (iff v (a <= b))

let ge =
  let a = (v : int) false in
  let b = (v : int) false in
  (v : bool) (iff v (a => b))

let plus =
  let a = (v : int) false in
  let b = (v : int) false in
  (v : int) (v == a + b)

let minus =
  let a = (v : int) false in
  let b = (v : int) false in
  (v : int) (v == a - b)

let nil = (v : int list) (fun (u : 'fa) -> not (mem v u))

let cons =
  let h = (v : int) false in
  let t = (v : int list) (fun (u : 'fa) -> implies (mem v u) (u == h)) in
  (v : int list)
    ((fun (u : 'ex) -> mem v u) && fun (u : 'fa) -> implies (mem v u) (u == h))

let _ret_two_value =
  let x = (v : int) (v > 0) in
  (v : int) (v == 1 || v == x)
