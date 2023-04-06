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
           (true, (goal (increment inv) false (increment inv)), inv,
             (goal (increment height) true (increment inv)))
       else
         if lt_eq_one inv
         then goal (increment height) c (increment inv)
         else
           Rbtnode
             (true, (goal (increment height) c (increment inv)),
               (increment (int_gen ())),
               (goal (increment (int_gen ())) true height)) : int rbtree)
