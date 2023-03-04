let rec merge (h1 : int leftisthp) (h2 : int leftisthp) : int leftisthp =
  match h1 with
  | Lhpleaf -> h2
  | Lhpnode (p1, x, a1, b1) -> (
      match h2 with
      | Lhpleaf -> h1
      | Lhpnode (p2, y, a2, b2) ->
          let (cond : bool) = x <= y in
          if cond then
            let (h3 : int leftisthp) = merge b1 h2 in
            let (h4 : int leftisthp) = makeT x a1 h3 in
            h4
          else
            let (h5 : int leftisthp) = merge h1 b2 in
            let (h6 : int leftisthp) = makeT y a2 h5 in
            h6)
