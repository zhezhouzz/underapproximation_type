let rec goal (inv : int) (c : bool) (height : int) =
  (if c
   then
     Rbtnode
       (true, (goal inv c inv), (increment inv),
         (goal inv true (increment r)))
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
           (true, (goal (increment height) true (increment inv)),
             (increment inv), (goal (increment height) c (int_gen ())))
       else
         if c
         then
           Rbtnode
             (true, (goal r c (int_gen ())), (increment (int_gen ())),
               (goal (int_gen ()) false height))
         else
           if c
           then
             Rbtnode
               (true, (goal height true (increment (int_gen ()))),
                 (increment height), (goal r c (increment height)))
           else
             Rbtnode
               (true, (goal (int_gen ()) false inv), inv,
                 (goal (increment inv) false (increment r))) : int rbtree)
