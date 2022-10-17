let[@library] int_range =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (a <= v : [%v: int]) in
  (a <= v && v <= b : [%v: int])

let[@library] bool_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: bool])

let bst_gen_v1 =
  let (lo : [%over: int]) = (true : [%v: int]) in
  let (hi : [%over: int]) = (lo <= v : [%v: int]) in
  (len v 0 : [%v: int tree])

let[@inv? n when 0] bst_gen_v2 =
  let (d : [%over: int]) = (0 <= v : [%v: int]) in
  let (lo : [%over: int]) = (true : [%v: int]) in
  let (hi : [%over: int]) = (v == lo + d : [%v: int]) in
  (fun (u : [%forall: int]) ->
     implies (mem v u) (lo < u && u < hi)
     && sorted v
     && implies (u < d) (len v u)
    : [%v: int tree])

(* let[@inv? n when 0] bst_gen_v3 = *)
(*   let (n : [%ghost: int]) = (0 <= v : [%v: int]) in *)
(*   let (lo : [%over: int]) = (true : [%v: int]) in *)
(*   let (hi : [%over: int]) = (v <= lo + n + 1 : [%v: int]) in *)
(*   ((fun (u : [%forall: int]) -> iff (mem v u) (lo < u && u < hi)) && sorted v *)
(*     : [%v: int tree]) *)

(* let[@inv? n when 0] bst_gen_v3 = *)
(*   let (n : [%ghost: int]) = (0 <= v : [%v: int]) in *)
(*   let (lo : [%over: int]) = (true : [%v: int]) in *)
(*   let (hi : [%over: int]) = (v <= lo + n : [%v: int]) in *)
(*   (fun (u : [%forall: int]) -> implies (mem v u) (lo < u && u < hi) && sorted v *)
(*     : [%v: int tree]) *)
