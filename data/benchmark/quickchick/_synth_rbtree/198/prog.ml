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
         (true, (goal (increment inv) true (int_gen ())),
           (increment (int_gen ())), Rbtleaf)
     else
       if c
       then
         Rbtnode
           (true, (goal (increment inv) false (increment inv)), inv,
             (goal (increment height) true (increment inv)))
       else
         if c
         then
           Rbtnode
             (true, (goal r true inv), (increment (int_gen ())),
               (goal (increment inv) true (increment r)))
         else
           if lt_eq_one inv
           then
             Rbtnode
               (true, (goal inv false inv), (increment (int_gen ())),
                 (goal (increment height) true (increment inv)))
           else
             Rbtnode
               (true, (goal r c (increment inv)), (increment inv),
                 (goal (int_gen ()) false (int_gen ()))) : int rbtree)
