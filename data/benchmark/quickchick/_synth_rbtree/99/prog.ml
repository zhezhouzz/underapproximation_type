let rec goal (inv : int) (c : bool) (height : int) =
  (if c
   then Rbtnode (true, (goal inv c inv), (int_gen ()), (goal inv c inv))
   else
     if c
     then
       Rbtnode
         (true, (goal (increment inv) true (int_gen ())),
           (increment (int_gen ())), Rbtleaf)
     else
       if c
       then
         Rbtnode
           (true, (goal (increment inv) false (increment inv)), inv,
             (goal (increment inv) false (increment inv)))
       else
         if lt_eq_one inv
         then goal (increment height) c (increment inv)
         else
           Rbtnode
             (true, (goal inv c (int_gen ())), (increment inv),
               (goal (increment (int_gen ())) true height)) : int rbtree)
