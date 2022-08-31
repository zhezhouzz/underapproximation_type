let[@notation] nil (u : 'forall * int) = (v : int list) (not (mem v u))

let[@notation] cons (u : 'forall * int) (w : 'forall * int) =
  let l (z : int) =
    (v : int list) (implies (mem v u && mem v w) (u == w) && mem v z)
  in
  ((h : int) (mem l h), (t : int list) (implies (mem t u) (mem l u)))

let[@notation] ileaf (u : 'forall * int) = (v : int_tree) (not (mem v u))

let[@notation] inode (u : 'forall * int) (w : 'forall * int) =
  let tree (z : int) =
    (v : int_tree) (implies (mem v u && mem v w) (u == w) && mem v z)
  in
  ( (root : int) (mem tree root),
    (left : int_tree) (implies (mem left u) (mem tree u)),
    (right : int_tree) (implies (mem right u) (mem tree u)) )
