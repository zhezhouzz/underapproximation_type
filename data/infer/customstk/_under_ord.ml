let concat =
  let l1 =
    (fun (u : [%forall: int]) (w : [%forall: int]) ->
       implies (ord v u w) (u <= w)
      : [%v: int list])
  in
  let l2 =
    (fun (u : [%forall: int]) (w : [%forall: int]) ->
       implies (ord v u w) (u <= w)
      : [%v: int list])
  in
  (* (fun (u : [%forall: int]) (w : [%forall: int]) -> implies (ord v u w) (u <= w) *)
  (*   : [%v: int list]) *)
  (true : [%v: int list])
