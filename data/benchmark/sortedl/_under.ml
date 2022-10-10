let[@library] int_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: int])

let[@inv? n when 0] sorted_list_gen =
  let (n : [%ghost: int]) = (0 <= v : [%v: int]) in
  let (s : [%over: int]) = (0 <= v && v == n : [%v: int]) in
  let (x : [%over: int]) = (true : [%v: int]) in
  (len v s && fun (u : [%forall: int]) (w : [%forall: int]) ->
   implies (mem v u) (x <= u) && implies (ord v u w) (u <= w)
    : [%v: int list])

(* let[@inv? n when 0] sorted_list_gen = *)
(*   let (n : [%ghost: int]) = (0 <= v : [%v: int]) in *)
(*   let (s : [%over: int]) = (0 <= v && v == n : [%v: int]) in *)
(*   let (x : [%over: int]) = (v > 0 : [%v: int]) in *)
(*   (len v s && fun (u : [%forall: int]) (w : [%forall: int]) -> *)
(*    implies (mem v u) (x <= u) && implies (ord v u w) (u <= w) *)
(*     : [%v: int list]) *)
