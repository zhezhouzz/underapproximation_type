let lemma1 (l : [%forall: int]) (u : [%forall: int]) =
  implies (empty l) (not (mem l u))

(* let lemma2 (l : [%forall: int list]) (u : [%exists: int]) = *)
(*   implies (not (empty l)) (mem l u) *)
