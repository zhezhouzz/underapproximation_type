let foo =
  let x = (v : int) true in
  let l = (v : int list) (mem v x) in
  (v : int list) (fun (u : 'fa) -> implies (mem v u) (mem l u))
