let[@library] gt_eq_int_gen =
  let (x : [%over: int]) = (true : [%v: int]) in
  (true : [%v: int])

let[@library] sizecheck =
  let (x : [%over: int]) = (true : [%v: int]) in
  (iff v (x == 0) && iff (not v) (x > 0) : [%v: bool])

let[@library] subs =
    let (s : [%over: int]) = (true : [%v: int]) in
    (v == (s - 1) : [%v: int])

let[@library] bool_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: bool])



let goal =
  let (s : [%over: int]) = (v >= 0 : [%v: int]) in
  let (x : [%over: int]) = (true : [%v: int]) in
  (len v s && fun (u : [%forall: int]) -> implies (mem v u) (u == x)
    : [%v: int list])

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
