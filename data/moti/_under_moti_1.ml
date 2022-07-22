let foo =
  let x = (v : int) true in
  let l = (v : int list) true in
  (v : int list) (fun (u : 'fa) -> implies (mem v u) (u == x))
