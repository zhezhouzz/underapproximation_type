let rec goal (inv : int) (c : bool) (height : int) =
  (if c
   then Rbtnode (true, (goal inv c inv), (int_gen ()), (goal inv c inv))
   else
     if c
     then
       Rbtnode
         (true, (goal (int_gen ()) c (increment (int_gen ()))), (int_gen ()),
           (goal r false inv))
     else
       if c
       then
         Rbtnode
           (true, (goal (increment inv) true (int_gen ())),
             (increment height), (goal (increment inv) c inv))
       else
         if c
         then
           Rbtnode
             (true, (goal r true inv), (increment inv),
               (goal (int_gen ()) false inv))
         else
           if c
           then
             Rbtnode
               (true, (goal inv c inv), (int_gen ()),
                 (goal r c (increment height)))
           else
             Rbtnode
               (true, (goal r c (int_gen ())), (increment height),
                 (goal inv true (increment inv))) : int rbtree)
