let eq =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a == b))

let neq =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a != b))

let lt =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a < b))

let gt =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a > b))

let le =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a <= b))

let ge =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a => b))

let plus =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : int) (v == a + b)

let minus =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : int) (v == a - b)

let nil =
  (v : int list) (fun (u : [%forall: int]) -> (not (mem v u)) && not (hd v u))

let cons =
  let h = (v : int) true in
  let t = (v : int list) true in
  (v : int list) (fun (u : [%forall: int]) ->
      iff (mem v u) (mem t u || u == h)
      && iff (hd v u) (u == h)
      && implies (hd v u) (mem v u))
