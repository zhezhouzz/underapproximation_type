let rec complete_tree_gen (s : int) : int tree = if s == 0 then Leaf else Err

let[@assert] complete_tree_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  (depth v s && complete v : [%v: int tree]) [@under]
