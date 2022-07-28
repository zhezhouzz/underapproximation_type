let lt =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a < b))

let intlistnil =
  (v : int list) (fun (u : 'fa) -> (not (mem v u)) && not (hd v u))

let intlistcons =
  let h = (v : int) true in
  let t = (v : int list) true in
  (v : int list) (fun (u : 'fa) ->
      iff (mem v u) (mem t u || u == h)
      && iff (hd v u) (u == h)
      && implies (hd v u) (mem v u))
