(* let rotate = *)
(*   let k = (true : [%v: int]) in *)
(*   let q1 = *)
(*     (fun (u : [%forall: int]) -> implies (mem v u) (u == k) : [%v: int list]) *)
(*   in *)
(*   let q2 = *)
(*     (fun (u : [%forall: int]) -> implies (mem v u) (u == k) : [%v: int list]) *)
(*   in *)
(*   let q3 = *)
(*     (fun (u : [%forall: int]) -> iff (mem v u) (u == k) : [%v: int list]) *)
(*   in *)
(*   (fun (u : [%forall: int]) -> implies (mem v u) (u == k) : [%v: int list]) *)

let rotate =
  let q1 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) : [%v: int list])
  in
  let q2 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd q1 u) : [%v: int list])
  in
  let q3 =
    (fun (u : [%forall: int]) -> iff (mem v u) (hd q1 u) : [%v: int list])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd q1 u) && not (empty v)
    : [%v: int list])
