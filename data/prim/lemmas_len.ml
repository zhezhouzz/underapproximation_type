let lemma1 (l : [%forall: int list]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let lemma2 (l : [%forall: int list]) = implies (len l 0) (empty l)
