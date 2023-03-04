let rec ins (x : int) (s : int rbset) : int rbset =
  match s with
  | Rbsleaf -> Rbsnode (true, Rbsleaf, x, Rbsleaf)
  | Rbsnode (color, a, y, b) ->
      let (cond1 : bool) = x < y in
      if cond1 then
        let (tmp0 : int rbset) = ins x a in
        let (tmp1 : int rbset) = balance color tmp0 y b in
        tmp1
      else
        let (cond2 : bool) = y < x in
        if cond2 then
          let (tmp2 : int rbset) = ins x b in
          let (tmp3 : int rbset) = balance color a y tmp2 in
          tmp3
        else s
