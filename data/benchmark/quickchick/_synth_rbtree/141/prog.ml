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
         (true, (goal (increment height) c height), (int_gen ()),
           (goal r false inv))
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
             (true, (goal r true inv), (increment (int_gen ())),
               (goal (increment inv) true (increment r)))
         else
           if c
           then
             Rbtnode
               (true, (goal (increment inv) false r),
                 (increment (int_gen ())), (goal inv c inv))
           else
             Rbtnode
               (true, (goal r c (int_gen ())), (increment height),
                 (goal inv true (increment inv))) : int rbtree)
