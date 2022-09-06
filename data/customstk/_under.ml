let concat =
  let l1 = (v : int list) (empty v) in
  let l2 = (v : int list) (empty v) in
  (v : int list) (empty v)

let concat (u : [%forall: int]) =
  let l1 = (v : int list) (implies (mem v u) (hd v u)) in
  let l2 = (v : int list) (implies (mem v u) (hd v u)) in
  (v : int list) (implies (mem v u) (hd v u))

let concat (u : [%forall: int]) =
  let l1 = (v : int list) (implies (mem v u) (hd v u)) in
  let l2 = (v : int list) (implies (mem v u) (hd v u)) in
  (v : int list)
    ((iff (mem v u) (mem l1 u || mem l2 u) && iff (hd v u) (hd l1 u || hd l2 u))
    && implies (mem v u) (hd v u))

let concat (u : [%forall: int]) =
  let l1 = (v : int list) (implies (mem v u) (hd v u)) in
  let l2 = (v : int list) (implies (mem v u) (hd v u)) in
  (v : int list)
    (iff (mem v u) (mem l1 u || mem l2 u) && iff (hd v u) (hd l1 u || hd l2 u))

let concat (u : [%forall: int]) =
  let l1 = (v : int list) (implies (mem v u) (hd v u)) in
  let l2 = (v : int list) (implies (mem v u) (hd v u)) in
  (v : int list) false

(* let concat (u : [%forall: int]) = *)
(*   let l1 = (v : int list) (implies (mem v u) (hd v u)) in *)
(*   let l2 = (v : int list) (implies (mem v u) (hd v u)) in *)
(*   (v : int list) (iff (mem v u) (mem l1 u || mem l2 u)) *)

(* let concat (u : [%forall: int]) = *)
(*   let l1 = (v : int list) (implies (mem v u) (hd v u)) in *)
(*   let l2 = (v : int list) (implies (mem v u) (hd v u)) in *)
(*   (v : int list) (iff (hd v u) (hd l1 u || hd l2 u)) *)
