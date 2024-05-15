(* let rec insert (tr : int tree) (x : int) : int tree = *)
(*   match tr with Leaf -> Node (x, Leaf, Leaf) | Node (y, l, r) -> Err *)

(* let[@assert] insert = *)
(*   let (i [@ghost]) = (1 == v : [%v: int]) [@over] in *)
(*   let s = (bst v && num_node v (i - 1) : [%v: int tree]) [@under] in *)
(*   let x = (true : [%v: int]) [@under] in *)
(*   (bst v && num_node v i : [%v: int tree]) [@under] *)

let rec insert (tr : int tree) (x : int) : int tree =
  match tr with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (y, l, r) ->
      if x == y then Node (y, l, r)
      else if x < y then Node (y, insert l x, r)
      else Node (y, l, insert r x)

let[@assert] insert =
  let (i [@ghost]) = (0 < v : [%v: int]) [@over] in
  let s = (bst v && num_node v (i - 1) : [%v: int tree]) [@under] in
  let x = (true : [%v: int]) [@under] in
  (bst v && num_node v i : [%v: int tree]) [@under]
