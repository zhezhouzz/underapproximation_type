let[@library] int_gen =
  let _ = (true : [%v: unit]) [@over] in
  (true : [%v: int]) [@under]

let sorted_list_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  let x = (true : [%v: int]) [@over] in
  (len v s && fun (u : [%forall: int]) (w : [%forall: int]) ->
   implies (mem v u) (x <= u) && implies (ord v u w) (u <= w)
    : [%v: int list])
    [@under]

(* let sorted_list_gen = *)
(*   let (s ) = (0 <= v : [%v: int]) [@over] in *)
(*   let (x ) = (v > 0 : [%v: int]) [@over] in *)
(*   (len v s && fun (u : [%forall: int]) (w : [%forall: int]) -> *)
(*    implies (mem v u) (x <= u) && implies (ord v u w) (u <= w) *)
(*     : [%v: int list]) [@under] *)
