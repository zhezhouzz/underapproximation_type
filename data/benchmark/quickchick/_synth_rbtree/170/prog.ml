let rec goal (inv : int) (c : bool) (height : int) =
  (if c
   then
     Rbtnode
       (true, (goal (int_gen ()) true inv), (int_gen ()),
         (goal (int_gen ()) false (int_gen ())))
   else
     if c
     then
       Rbtnode
         (true, (goal (int_gen ()) c (increment (int_gen ()))), (int_gen ()),
           (goal r false inv))
     else
       if lt_eq_one inv
       then goal (increment height) true (increment inv)
       else
         Rbtnode
           (true, (goal (int_gen ()) c (increment (int_gen ()))),
             (increment height),
             (goal (increment (int_gen ())) c (increment inv))) : int rbtree)
