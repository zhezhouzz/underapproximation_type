let lt =
  let a = (v : int) true in
  let b = (v : int) true in
  (v : bool) (iff v (a < b))

let intlistnil = (v : int list) (fun (u : 'fa) -> not (mem v u))
let rev_intlistnil = (v : int list) (fun (u : 'fa) -> not (mem v u))

let intlistcons =
  let h = (v : int) true in
  let t = (v : int list) (fun (u : 'fa) -> implies (mem v u) (u = h)) in
  (v : int list)
    ((fun (u : 'ex) -> mem v u) && fun (u : 'fa) -> implies (mem v u) (u = h))

let rev_intlistcons =
  let l =
    (v : int list) (fun (u : 'ex) ->
        mem v u && fun (w : 'fa) -> implies (mem v w) (w = u))
  in
  ( (h : int) (mem l h),
    (t : int list) (fun (u : 'fa) -> implies (mem t u) (mem l u)) )
