let rec contains (c : natoptionnatoption) (n : int) : bool =
  match c with
  | NoneNone -> false
  | NoneSome b -> false
  | SomeNone a -> a == n
  | SomeSome (a, b) -> a == n || b == n
