let[@library] merge0 =
  let l1 = (fun (u : [%forall: int]) -> empty v : [%v: int list]) in
  let l2 =
    (fun (u : [%forall: int]) (w : [%forall: int]) ->
       implies (ord v u w) (u <= w) && not (empty v)
      : [%v: int list])
  in
  (fun (u : [%forall: int]) (w : [%forall: int]) ->
     implies (ord v u w) (u <= w) && not (empty v)
    : [%v: int list])

(* implies (ord v u w) (u <= w) ==> *)
(* implies (mem v u && mem v w) (u == w) ==> *)
(* implies (mem v u && mem v w && not (hd v u)) (u == w) && iff (hd v u) (u == h1) ==> *)

let merge =
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
  (fun (u : [%forall: int]) (w : [%forall: int]) ->
     implies (hd l1 u && mem l2 w) (ord v u w)
     (* && implies (hd l1 u && hd l2 w) (w < u) *)
     && iff (hd l1 u) (hd v u)
     (* && implies (hd v u) (not (ord v u u)) *)
     && implies (ord v u w && not (hd v u)) (u <= w)
     && (not (empty l1))
     && not (empty l2)
    : [%v: int list])
