let merge (h1 : int pairinghp) (h2 : int pairinghp) : int pairinghp =
  match h1 with
  | Phpleaf -> h2
  | Phpnode (x, hs1) -> (
      match h2 with
      | Phpleaf -> h1
      | Phpnode (y, hs2) ->
          let (b : bool) = x <= y in
          if b then
            let (tmp0 : int pairinghp list) = h2 :: hs1 in
            let (tmp1 : int pairinghp) = Phpnode (x, tmp0) in
            tmp1
          else
            let (tmp2 : int pairinghp list) = h1 :: hs2 in
            let (tmp3 : int pairinghp) = Phpnode (y, tmp2) in
            tmp3)
