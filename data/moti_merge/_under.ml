let merge =
  let h1 = (true : [%v: int]) in
  let h2 = (true : [%v: int]) in
  let l1 =
    (fun (u : [%forall: int]) (w : [%forall: int]) ->
       implies (ord v u w) (u <= w) && implies (hd v u) (h1 <= u)
      : [%v: int list])
  in
  let l2 =
    (fun (u : [%forall: int]) (w : [%forall: int]) ->
       implies (ord v u w) (u <= w) && hd v h2
      : [%v: int list])
  in
  (fun (u : [%forall: int]) (w : [%forall: int]) ->
     implies (ord v u w) (u <= w) && hd v h2
    : [%v: int list])
