let rec contains_full (tr : int tree) (x : int) : bool =
  match tr with
  | Leaf -> false
  | Node (y, l, r) -> x == y || contains_full l x || contains_full r x
