let div (n1 : int) (n2 : int) : natnatoption =
  if n2 == 0 then NonePair else SomePair (n1, n2)
