let rotate =
  let q1 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) : [%v: int list])
  in
  let q2 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd q1 u) : [%v: int list])
  in
  let q3 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd q1 u) : [%v: int list])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd q1 u) : [%v: int list])
