let rec goal (inv : int) (c : bool) (height : int) =
  (if c
   then
     Rbtnode
       (true, (goal (int_gen ()) true inv), (increment inv),
         (goal (increment inv) false (increment height)))
   else
     if lt_eq_one inv
     then goal (increment inv) false (increment inv)
     else
       Rbtnode
         (true, (goal (increment (int_gen ())) c inv), inv,
           (goal (increment (int_gen ())) c inv)) : int rbtree)
