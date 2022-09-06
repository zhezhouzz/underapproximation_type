let[@notation] nil (u : [%forall: int]) = (v : int list) (not (mem v u))

let[@notation] cons (u : [%forall: int]) (w : [%forall: int]) =
  let l =
    (v : int list) (implies (mem v u && mem v w) (u == w) && not (empty v))
  in
  ((h : int) (mem l h), (t : int list) (implies (mem t u && mem t w) (u == w)))

let[@notation] ileaf (u : [%forall: int]) = (v : int_tree) (not (mem v u))

let[@notation] inode (u : [%forall: int]) (w : [%forall: int]) =
  let tree =
    (v : int_tree) (implies (mem v u && mem v w) (u == w) && not (empty v))
  in
  ( (root : int) (mem tree root),
    (left : int_tree) (implies (mem left u && mem left w) (u == w)),
    (right : int_tree) (implies (mem right u && mem right w) (u == w)) )
