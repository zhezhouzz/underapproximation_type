let list_gen =
  let s = (v >= 0 : [%v: int]) [@over] in
  let x = (true : [%v: int]) [@over] in
  (len v s && fun (u : [%forall: int]) -> implies (mem v u) (u == x)
    : [%v: int list])
    [@under]

(* Wrong measure type *)
(* let[@inv? n when 0] list_gen = *)
(*   let (n ) = (v >= 1 : [%v: int]) [@over] in *)
(*   let (s ) = (v >= 0 && v == n : [%v: int]) [@over] in *)
(*   let (x ) = (true : [%v: int]) [@over] in *)
(*   (len v s && fun (u : [%forall: int]) -> implies (mem v u) (u == x) *)
(*     : [%v: int list]) *)

(* Wrong return type *)
(* let[@inv? n when 0] list_gen = *)
(*   let (n ) = (v >= 0 : [%v: int]) [@over] in *)
(*   let (s ) = (v >= 0 && v == n : [%v: int]) [@over] in *)
(*   let (x ) = (true : [%v: int]) [@over] in *)
(*   (len v s : [%v: int list]) *)
