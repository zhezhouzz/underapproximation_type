let empty = Leaf

let[@assert] empty =
  let (i [@ghost]) = (0 == v : [%v: int]) [@over] in
  (bst v && num_node v i : [%v: int tree]) [@under]
