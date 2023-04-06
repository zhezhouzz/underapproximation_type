let rec goal (inv : int) (c : bool) (height : int) =
  (if c
   then Rbtnode (true, (goal inv c inv), (int_gen ()), (goal inv c inv))
   else
     if c
     then
       Rbtnode
         (true, (goal (increment height) c height), (int_gen ()),
           (goal r false inv))
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
             (true, (goal r true inv), (increment inv),
               (goal (int_gen ()) false inv))
         else
           if lt_eq_one height
           then goal (increment inv) true (increment inv)
           else
             Rbtnode
               (true, (goal (increment inv) true (increment inv)),
                 (increment inv), (goal (increment inv) false height)) : 
  int rbtree)
