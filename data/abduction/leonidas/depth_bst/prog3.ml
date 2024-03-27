let rec size_bst_gen (d : int) (lo : int) (hi : int) : int tree =
  if d == 0 then Leaf
  else if bool_gen () then Leaf
  else if lo + 1 < hi then Err
  else Exn

let[@assert] size_bst_gen =
  let d = (0 <= v : [%v: int]) [@over] in
  let lo = (true : [%v: int]) [@over] in
  let hi = (lo < v : [%v: int]) [@over] in
  ((fun (u : int) -> (tree_mem v u) #==> (lo < u && u < hi))
   && bst v
   && fun ((n [@exists]) : int) -> depth v n && n <= d
    : [%v: int tree])
    [@under]
