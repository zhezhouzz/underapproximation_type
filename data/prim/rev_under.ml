let[@library] nil = (fun (u : [%forall: int]) -> not (mem v u) : [%v: int list])

let[@library] cons =
  let l =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) && not (empty v)
      : [%v: int list])
  in
  ( h (fun (u : [%forall: int]) -> iff (hd l u) (v == u) : [%v: int]),
    t (fun (u : [%forall: int]) -> iff (mem v u) (mem l u) : [%v: int list]) )

let[@library] ileaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int_tree])

let[@library] inode =
  let tree =
    (fun (u : [%forall: int]) (w : [%forall: int]) ->
       implies (mem v u && mem v w) (u == w) && not (empty v)
      : [%v: int_tree])
  in
  ( root (fun (u : [%forall: int]) -> mem tree v : [%v: int]),
    left
      (fun (u : [%forall: int]) -> implies (mem v u) (mem tree u)
        : [%v: int_tree]),
    right
      (fun (u : [%forall: int]) -> implies (mem v u) (mem tree u)
        : [%v: int_tree]) )
