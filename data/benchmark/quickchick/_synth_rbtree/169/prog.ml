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
               (increment inv), (goal (int_gen ()) false height))
         else
           if c
           then
             Rbtnode
               (true, (goal inv true (increment inv)), (increment inv),
                 (goal height true (increment (int_gen ()))))
           else
             Rbtnode
               (true, (goal r true inv), (increment height),
                 (goal (increment (int_gen ())) true (increment inv))) : 
  int rbtree)
