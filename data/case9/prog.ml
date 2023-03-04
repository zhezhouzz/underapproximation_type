let foo (t : int tree) : int tree =
  match t with Leaf -> Leaf | Node (a, left, right) -> left
