let rec contains_fast (tr : int tree) (x : int) : bool =
  match tr with Leaf -> false | Node (y, l, r) -> x == y || contains_fast r x
