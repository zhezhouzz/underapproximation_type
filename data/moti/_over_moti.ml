let foo =
  let x = (v : int) true in
  let l = (v : int list) true in
  (v : int list) (fun (u : 'fa) -> iff (mem v u) (mem l u || u == x))
