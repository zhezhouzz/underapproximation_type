let insert (c : natoptionnatoption) (n : int) : natoptionnatoption =
  match c with
  | NoneNone -> SomeNone n
  | NoneSome b -> SomeNone n
  | SomeNone a -> if n < a then SomeSome (n, a) else SomeNone n
  | SomeSome (a, b) -> if n < a then SomeSome (n, a) else SomeNone n
