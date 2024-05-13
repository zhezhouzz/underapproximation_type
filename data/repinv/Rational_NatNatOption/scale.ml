let scale (n : int) (r : natnatoption) =
  match r with NonePair -> NonePair | SomePair (a, b) -> SomePair (n * a, b)
