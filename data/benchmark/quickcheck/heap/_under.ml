external method_predicates : t = "len" "heap" "hd" "<="

(* let[@library] int_gen = *)
(*   let _ = (true : [%v: unit]) [@over] in *)
(*   (true : [%v: int]) [@under] *)

(* let[@library] bool_gen = *)
(*   let _ = (true : [%v: unit]) [@over] in *)
(*   (true : [%v: bool]) [@under] *)

let heap_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  let max = (true : [%v: int]) [@over] in
  (heap v && len v s && fun (u : [%forall: int]) -> implies (hd v u) (u <= max)
    : [%v: int heap])
    [@under]
