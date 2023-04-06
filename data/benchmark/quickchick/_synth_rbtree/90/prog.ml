let rec goal (inv : int) (c : bool) (height : int) =
  (if c
   then Rbtnode (true, (goal inv c inv), (int_gen ()), (goal inv c inv))
   else
     if lt_eq_one inv
     then goal (increment inv) false (increment inv)
     else
       Rbtnode
         (true, (goal (increment (int_gen ())) c height), (increment height),
           (goal (increment (int_gen ())) c inv)) : int rbtree)
