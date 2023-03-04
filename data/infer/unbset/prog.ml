let rec insert (x : int) (s : int unbset) : int unbset =
  match s with
  | Usleaf -> Usnode (Usleaf, x, Usleaf)
  | Usnode (a, y, b) ->
      let (cond1 : bool) = x < y in
      if cond1 then
        let (tmp0 : int unbset) = insert x a in
        let (tmp1 : int unbset) = Usnode (tmp0, y, b) in
        tmp1
      else
        let (cond2 : bool) = y < x in
        if cond2 then
          let (tmp2 : int unbset) = insert x b in
          let (tmp3 : int unbset) = Usnode (a, y, tmp2) in
          tmp3
        else s
