let rec rbtree_gen (inv : int) (color : bool) (h : int) : int rbtree =
  if h == 0 then
    if color then Rbtleaf
    else if bool_gen () then Rbtleaf
    else Rbtnode (true, Rbtleaf, int_gen (), Rbtleaf)
  else
    let (hh : int) = h - 1 in
    let (rt : int) = int_gen () in
    if color then
      let (lt2 : int rbtree) = rbtree_gen (inv - 1) false hh in
      let (rt2 : int rbtree) = rbtree_gen (inv - 1) false hh in
      Rbtnode (false, lt2, rt, rt2)
    else
      let (c : bool) = bool_gen () in
      if c then
        let (lt3 : int rbtree) = rbtree_gen (inv - 1) true h in
        let (rt3 : int rbtree) = rbtree_gen (inv - 1) true h in
        Rbtnode (true, lt3, rt, rt3)
      else
        let (lt4 : int rbtree) = rbtree_gen (inv - 2) false hh in
        let (rt4 : int rbtree) = rbtree_gen (inv - 2) false hh in
        Rbtnode (false, lt4, rt, rt4)

let[@assert] rbtree_gen =
  let inv = (v >= 0 : [%v: int]) [@over] in
  let c = (true : [%v: bool]) [@over] in
  let[@assert] h =
    (v >= 0 && if c then v + v == inv else v + v + 1 == inv : [%v: int]) [@over]
  in
  (num_black v h && no_red_red v
   &&
   if c then not (rb_root_color v true)
   else (h == 0) #==> (rb_root_color v true)
    : [%v: int rbtree])
    [@under]
