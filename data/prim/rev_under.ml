let nil = (v : int list) (fun (u : 'fa) -> not (mem v u))

let cons =
  let l =
    (v : int list) (fun (u : 'ex) ->
        mem v u && fun (w : 'fa) -> implies (mem v w) (w == u))
  in
  ( (h : int) (mem l h),
    (t : int list) (fun (u : 'fa) -> implies (mem t u) (mem l u)) )
