let rec depth_tree_gen (s : int) : int tree =
  if sizecheck s then Leaf else if bool_gen () then Leaf else Err

let[@assert] depth_tree_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  (fun ((u [@exists]) : int) -> depth v u && u <= s : [%v: int tree]) [@under]
