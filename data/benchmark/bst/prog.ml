let bst_gen_v1 (lo : int) (hi : int) : int tree = Leaf

(* let rec bst_gen_v2 (lo : int) (hi : int) : int tree = *)
(*   if lo + 1 >= hi then Leaf *)
(*   else *)
(*     let (lo1 : int) = lo + 1 in *)
(*     let (hi1 : int) = hi - 1 in *)
(*     let (x : int) = int_range lo1 hi1 in *)
(*     let (lt : int tree) = bst_gen_v2 lo x in *)
(*     let (rt : int tree) = bst_gen_v2 x hi in *)
(*     Node (x, lt, rt) *)

let rec bst_gen_v2 (diff : int) (lo : int) (hi : int) : int tree =
  if diff <= 1 then Leaf
  else if bool_gen () then Leaf
  else
    let (lo1 : int) = lo + 1 in
    let (hi1 : int) = hi - 1 in
    let (x : int) = int_range lo1 hi1 in
    let (diff1 : int) = x - lo in
    let (lt : int tree) = bst_gen_v2 diff1 lo x in
    let (diff2 : int) = hi - x in
    let (rt : int tree) = bst_gen_v2 diff2 x hi in
    Node (x, lt, rt)

(* let rec bst_gen_v3 (lo : int) (hi : int) : int tree = *)
(*   if lo + 1 >= hi then Leaf *)
(*   else *)
(*     let (lo1 : int) = lo + 1 in *)
(*     let (hi1 : int) = hi - 1 in *)
(*     let (x : int) = int_range lo1 hi1 in *)
(*     let (lt : int tree) = bst_gen_v3 lo x in *)
(*     let (rt : int tree) = bst_gen_v3 x hi in *)
(*     Node (x, lt, rt) *)

(* let rec bst_gen_v3 (lo : int) (hi : int) : int tree = *)
(*   if lo >= hi then Leaf *)
(*   else if bool_gen () then Leaf *)
(*   else *)
(*     let (x : int) = int_range lo (hi - 1) in *)
(*     let (lt : int tree) = bst_gen_v3 lo x in *)
(*     let (rt : int tree) = bst_gen_v3 x hi in *)
(*     Node (x, lt, rt) *)
