let rec goal (inv : int) (c : bool) (height : int) =
  (if c
   then Rbtnode (true, (goal inv c inv), (int_gen ()), (goal inv c inv))
   else
     if c
     then
       Rbtnode
         (true, (goal (increment height) true (increment (int_gen ()))),
           (int_gen ()), (goal (increment height) c height))
     else
       if lt_eq_one inv
       then goal (increment height) true (increment inv)
       else
         Rbtnode
           (true, (goal (int_gen ()) c (increment (int_gen ()))),
             (increment height),
             (goal (increment (int_gen ())) c (increment inv))) : int rbtree)
