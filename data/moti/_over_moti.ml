let foo =
  let x = (v : int) true in
  let l = (v : int list) true in
  (v : int list) (fun (u : 'fa) ->
      implies (u != x && not (mem l u)) (not (mem v u)))
