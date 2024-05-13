let bind (r : natnatoption) (f : int -> int -> natnatoption) =
  match r with NonePair -> NonePair | SomePair (a, b) -> f a b

(* (r1 : natnatoption) (r2 : natnatoption): natnatoption = *)
(*                                          match r1 with *)
(*                                          | NonePair -> NonePair *)
(*                                          | Some (a1, b1) -> *)
(*                                            match r2 with *)
(*                                            | NonePair -> NonePair *)
(*                                            | Some (a2, b2) ->  *)
