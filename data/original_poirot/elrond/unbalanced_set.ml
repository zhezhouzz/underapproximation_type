let rec unbalanced_set_gen (d : int) (diff : int) (lo : int) (hi : int) :
    int tree =
  if d == 0 then Leaf
  else if bool_gen () then Leaf
  else if lo + 1 < hi then
    let (x : int) = int_range lo hi in
    let (lt : int tree) = unbalanced_set_gen (d - 1) (x - lo) lo x in
    let (rt : int tree) = unbalanced_set_gen (d - 1) (hi - x) x hi in
    Node (x, lt, rt)
  else Exn

let[@assert] unbalanced_set_gen =
  let d = (0 <= v : [%v: int]) [@over] in
  let diff = (true : [%v: int]) [@over] in
  let lo = (true : [%v: int]) [@over] in
  let hi = (lo < v : [%v: int]) [@over] in
  ((fun (u : int) -> (tree_mem v u) #==> (lo < u && u < hi))
   && bst v
   && fun ((n [@exists]) : int) -> depth v n && n <= d
    : [%v: int tree])
    [@under]
