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
       if c
       then
         Rbtnode
           (true, (goal (increment height) true (increment inv)),
             (increment inv), (goal (increment height) c (int_gen ())))
       else
         if c
         then
           Rbtnode
             (true, (goal (increment inv) false (increment inv)),
               (increment inv), (goal (int_gen ()) false height))
         else
           if lt_eq_one inv
           then
             Rbtnode
               (true, (goal inv c (increment (int_gen ()))), (increment inv),
                 (goal (int_gen ()) c inv))
           else
             Rbtnode
               (true, (goal (increment inv) true (increment inv)),
                 (increment inv), (goal r true (int_gen ()))) : int rbtree)
