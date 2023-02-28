external method_predicates : t = "len" "complete"

(* let[@library] int_gen = *)
(*   let _ = (true : [%v: unit]) [@over] in *)
(*   (true : [%v: int]) [@under] *)

(* let[@library] bool_gen = *)
(*   let _ = (true : [%v: unit]) [@over] in *)
(*   (true : [%v: bool]) [@under] *)

let complete_tree_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  (len v s && complete v : [%v: int ctree]) [@under]
