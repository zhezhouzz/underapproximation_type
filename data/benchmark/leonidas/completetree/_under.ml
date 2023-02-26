let[@library] int_gen =
  let _ = (true : [%v: unit]) [@over] in
  (true : [%v: int]) [@under]

let[@library] bool_gen =
  let _ = (true : [%v: unit]) [@over] in
  (true : [%v: bool]) [@under]

let complete_tree_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  (len v s && complete v : [%v: int tree]) [@under]

(* should wrong *)
(* let complete_tree_gen = *)
(*   let (s ) = (0 <= v : [%v: int]) [@over] in *)
(*   (complete v && fun (u : [%forall: int]) -> implies (len v u) (0 <= u && u <= s) *)
(*     : [%v: int tree]) *)
