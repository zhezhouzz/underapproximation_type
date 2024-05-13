let delete (c : natoptionnatoption) (n : int) : natoptionnatoption =
  match c with
  | NoneNone -> NoneNone
  | NoneSome b -> if b == n then NoneNone else NoneSome b
  | SomeNone a -> if a == n then NoneNone else SomeNone a
  | SomeSome (a, b) ->
      if a == n then NoneNone
      else if a < n then if b == n then SomeNone a else SomeSome (a, b)
      else SomeSome (a, b)
