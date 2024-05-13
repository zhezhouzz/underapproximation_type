let rec size (tr : int tree) : int =
  match tr with Leaf -> 0 | Node (y, l, r) -> 1 + size l + size r
