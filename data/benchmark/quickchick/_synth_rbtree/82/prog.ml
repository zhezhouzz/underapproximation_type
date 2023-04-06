let rec goal (inv : int) (c : bool) (height : int) =
  (if c
   then
     Rbtnode
       (true, (goal (int_gen ()) true inv), (increment height),
         (goal (increment inv) false (increment height)))
   else
     if lt_eq_one height
     then goal (increment inv) false (increment inv)
     else goal (increment inv) false (increment (int_gen ())) : int rbtree)
