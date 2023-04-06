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
           (true, (goal (increment height) c (int_gen ())), (int_gen ()),
             (goal (increment inv) c inv))
       else
         if c
         then
           Rbtnode
             (true, (goal r true inv), (increment inv),
               (goal (int_gen ()) false inv))
         else
           if lt_eq_one inv
           then
             Rbtnode
               (true, (goal (int_gen ()) true (increment inv)),
                 (increment inv), (goal inv false (increment inv)))
           else
             Rbtnode
               (true, (goal inv true (increment (int_gen ()))),
                 (increment height), (goal (int_gen ()) false (int_gen ()))) : 
  int rbtree)
