external method_predicates : t = "len"

(* let[@library] int_range_inc = *)
(*   let a = (true : [%v: int]) [@over] in *)
(*   let b = (a <= v : [%v: int]) [@over] in *)
(*   (a <= v && v <= b : [%v: int]) [@under] *)

let[@library] stream_gen =
  let a = (true : [%v: int]) [@over] in
  (fun (u : [%forall: int]) -> implies (0 <= u && u <= a) (len v u)
    : [%v: int stream])
    [@under]

let bankersq_gen =
  let s = (v >= 0 : [%v: int]) [@over] in
  (len v s : [%v: int bankersq]) [@under]
