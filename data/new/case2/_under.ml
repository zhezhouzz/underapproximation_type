let list_gen =
  let (s : [%over: int]) = (v >= 0 : [%v: int]) in
  let (x : [%over: int]) = (true : [%v: int]) in
  (len v s && fun (u : [%forall: int]) -> implies (mem v u) (u == x)
    : [%v: int list])

(* Wrong measure type *)
(* let[@inv? n when s] list_gen = *)
(*   let (s : [%over: int]) = (v >= 0 : [%v: int]) in *)
(*   let (x : [%over: int]) = (true : [%v: int]) in *)
(*   let (n : [%ghost: int]) = (v == s && v >= 1 : [%v: int]) in *)
(*   (len v s && fun (u : [%forall: int]) -> implies (mem v u) (u == x) *)
(*     : [%v: int list]) *)

(* Wrong return type *)
let list_gen =
  let (s : [%over: int]) = (v >= 0 : [%v: int]) in
  let (x : [%over: int]) = (true : [%v: int]) in
  (len v s : [%v: int list])
