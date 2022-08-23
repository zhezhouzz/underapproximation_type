let foo (t : int_tree) =
  match t with ILeaf -> ILeaf | INode (a, left, right) -> left
