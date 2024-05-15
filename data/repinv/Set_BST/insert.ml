let rec insert (tr : elem tree) (x : elem) : elem tree =
  match tr with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (y, l, r) ->
      if elem_eq x y then Node (y, l, r)
      else if elem_lt x y then Node (y, insert l x, r)
      else Node (y, l, insert r x)

let[@assert] insert =
  let (i [@ghost]) = (0 < v : [%v: int]) [@over] in
  let s = (bst v && num_node v (i - 1) : [%v: elem tree]) [@under] in
  let x = (true : [%v: elem]) [@under] in
  (bst v && num_node v i : [%v: elem tree]) [@under]
