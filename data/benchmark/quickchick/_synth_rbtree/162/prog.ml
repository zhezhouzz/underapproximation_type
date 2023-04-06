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
         (true, (goal (increment height) c height), (increment (int_gen ())),
           (goal (int_gen ()) c height))
     else
       if c
       then
         Rbtnode
           (true, (goal (increment inv) false (increment inv)), inv,
             (goal (increment inv) false (increment inv)))
       else
         if c
         then
           Rbtnode
             (true, (goal (increment inv) false (increment inv)),
               (increment inv), (goal r c inv))
         else
           if c
           then
             Rbtnode
               (true, (goal height true (increment (int_gen ()))),
                 (increment height), (goal r c (increment height)))
           else
             Rbtnode
               (true, (goal height true (increment inv)), (increment height),
                 (goal (increment inv) false (increment r))) : int rbtree)
