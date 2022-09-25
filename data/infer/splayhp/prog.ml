let rec partition (pivot : int) (t : int splayhp) : int splayhp * int splayhp =
  match t with
  | Sphpleaf -> (Sphpleaf, Sphpleaf)
  | Sphpnode (a, x, b) -> (
      let (cond1 : bool) = x <= pivot in
      if cond1 then
        match b with
        | Sphpleaf -> (t, Sphpleaf)
        | Sphpnode (b1, y, b2) ->
            let (cond2 : bool) = y <= pivot in
            if cond2 then
              let (small : int splayhp), (big : int splayhp) =
                partition pivot b2
              in
              (Sphpnode (Sphpnode (a, x, b1), y, small), big)
            else
              let (small : int splayhp), (big : int splayhp) =
                partition pivot b1
              in
              (Sphpnode (a, x, small), Sphpnode (big, y, b2))
      else
        match a with
        | Sphpleaf -> (Sphpleaf, t)
        | Sphpnode (a1, y, a2) ->
            let (cond3 : bool) = y <= pivot in
            if cond3 then
              let (small : int splayhp), (big : int splayhp) =
                partition pivot a2
              in
              (Sphpnode (a1, y, small), Sphpnode (big, x, b))
            else
              let (small : int splayhp), (big : int splayhp) =
                partition pivot a1
              in
              (small, Sphpnode (big, y, Sphpnode (a2, x, b))))
