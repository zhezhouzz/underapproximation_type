let rec goal (inv : int) (c : bool) (height : int) =
  (if c
   then
     Rbtnode
       (true, (goal (int_gen ()) true inv), (increment inv),
         (goal (increment inv) false (increment height)))
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
           (true, (goal (increment height) c (int_gen ())), (int_gen ()),
             (goal (increment inv) c inv))
       else
         if c
         then
           Rbtnode
             (true, (goal r c (int_gen ())), (increment (int_gen ())),
               (goal (int_gen ()) false height))
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
