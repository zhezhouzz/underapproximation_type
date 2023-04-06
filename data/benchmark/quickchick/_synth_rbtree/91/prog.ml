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
           (true, (goal (increment height) true (increment inv)),
             (increment inv), (goal (increment height) c (int_gen ())))
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
               (true, (goal (increment inv) false r), (increment inv),
                 (goal (increment inv) false r))
           else
             Rbtnode
               (true, (goal (int_gen ()) false inv), inv,
                 (goal (increment inv) false (increment r))) : int rbtree)
