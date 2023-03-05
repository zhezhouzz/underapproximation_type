external method_predicates : t = "mem" "len" ">="



let goal =

  let s = (v >= 0 : [%v: int]) [@over] in
  let x = (true : [%v: int]) [@over] in
  (len v s && fun (u : [%forall: int]) -> implies (mem v u) (u == x)
    : [%v: int ulist])
    [@under]

  
(* Wrong measure type *)
(* let[@inv? n when 0] list_gen = *)
(*   let (n : [%ghost: int]) = (v >= 1 : [%v: int]) in *)
(*   let (s : [%over: int]) = (v >= 0 && v == n : [%v: int]) in *)
(*   let (x : [%over: int]) = (true : [%v: int]) in *)
(*   (len v s && fun (u : [%forall: int]) -> implies (mem v u) (u == x) *)
(*     : [%v: int list]) *)

(* Wrong return type *)
(* let[@inv? n when 0] list_gen = *)
(*   let (n : [%ghost: int]) = (v >= 0 : [%v: int]) in *)
(*   let (s : [%over: int]) = (v >= 0 && v == n : [%v: int]) in *)
(*   let (x : [%over: int]) = (true : [%v: int]) in *)
(*   (len v s : [%v: int list]) *)
