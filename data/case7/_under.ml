(* Should fail *)

let foo =
  let l = (true : [%v: int list]) in
  (fun (u : [%forall: int]) -> (not (empty v)) && implies (mem v u) (hd l u)
    : [%v: int list])

(* let foo = *)
(*   let l = (true : [%v: int list]) in *)
(*   (fun (u : [%forall: int]) -> implies (mem v u) (hd l u) : [%v: int list]) *)

(* let foo = *)
(*   let l = (true : [%v: int list]) in *)
(*   (fun (u : [%forall: int]) -> true : [%v: int list]) *)
