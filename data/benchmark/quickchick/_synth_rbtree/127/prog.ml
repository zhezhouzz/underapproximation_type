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
             (true, (goal r c (int_gen ())), (increment (int_gen ())),
               (goal (int_gen ()) false height))
         else
           if c
           then
             Rbtnode
               (true, (goal inv c inv), (int_gen ()),
                 (goal r c (increment height)))
           else
             Rbtnode
               (true, (goal r true inv), (increment height),
                 (goal (increment (int_gen ())) true (increment inv))) : 
  int rbtree)
