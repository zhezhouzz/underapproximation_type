let rec ranged_set_gen (diff : int) (lo : int) (hi : int) : int tree =
  if hi <= 1 + lo then Err
  else if bool_gen () then Leaf
  else
    let (x : int) = int_range lo hi in
    let (lt : int tree) = ranged_set_gen (x - lo) lo x in
    let (rt : int tree) = ranged_set_gen (hi - x) x hi in
    Node (x, lt, rt)

let[@assert] ranged_set_gen =
  let d = (0 <= v : [%v: int]) [@over] in
  let lo = (true : [%v: int]) [@over] in
  let hi = (v == lo + d : [%v: int]) [@over] in
  ((fun (u : int) -> (tree_mem v u) #==> (lo < u && u < hi)) && bst v
    : [%v: int tree])
    [@under]
