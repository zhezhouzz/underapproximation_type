let[@notation] nil = (v : int list) (empty v)

let[@notation] cons (u : [%forall: int]) =
  let l = (v : int list) (implies (mem v u) (hd v u) && not (empty v)) in
  ((h : int) (hd l h), (t : int list) (implies (mem t u) (hd t u)))
