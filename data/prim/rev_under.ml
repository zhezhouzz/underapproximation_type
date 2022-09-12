let[@notation] nil = (v : int list) (fun (u : [%forall: int]) -> not (mem v u))

let[@notation] cons =
  let l =
    (v : int list) (fun (u : [%forall: int]) (w : [%forall: int]) ->
        implies (mem v u && mem v w) (u == w) && not (empty v))
  in
  ( (v : int) (hd l v),
    (v : int list) (fun (u : [%forall: int]) -> implies (mem v u) (mem l u)) )

let[@notation] ileaf =
  (v : int_tree) (fun (u : [%forall: int]) -> not (mem v u))

let[@notation] inode =
  let tree =
    (v : int_tree) (fun (u : [%forall: int]) (w : [%forall: int]) ->
        implies (mem v u && mem v w) (u == w) && not (empty v))
  in
  ( (v : int) (fun (u : [%forall: int]) -> mem tree v),
    (v : int_tree) (fun (u : [%forall: int]) -> implies (mem v u) (mem tree u)),
    (v : int_tree) (fun (u : [%forall: int]) -> implies (mem v u) (mem tree u))
  )
