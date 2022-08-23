type int_tree = ILeaf | INode of int * int_tree * int_tree
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
