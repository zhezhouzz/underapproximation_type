let rec insert (tr : int tree) (x : int) : int tree =
  match tr with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (y, l, r) -> Node (y, l, insert r x)
