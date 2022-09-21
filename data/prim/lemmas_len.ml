let lemma1 (l : [%forall: int list]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

(* let lemma2 (l : [%forall: int list]) (u : [%exists: int]) = len l u *)
