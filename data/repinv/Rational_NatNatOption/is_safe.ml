let is_safe (r : natnatoption) : bool =
  match r with NonePair -> false | SomePair (a, b) -> true
