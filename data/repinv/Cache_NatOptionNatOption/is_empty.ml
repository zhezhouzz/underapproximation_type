let is_empty (c : natoptionnatoption) =
  match c with
  | NoneNone -> true
  | NoneSome b -> false
  | SomeNone a -> false
  | SomeSome (a, b) -> false
