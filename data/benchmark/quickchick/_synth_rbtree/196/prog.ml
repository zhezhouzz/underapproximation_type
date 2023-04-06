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
         (true, (goal (increment height) true (increment (int_gen ()))),
           (int_gen ()), (goal (increment height) c height))
     else
       if lt_eq_one inv
       then goal (increment height) true (increment inv)
       else
         Rbtnode
           (true, (goal (increment (int_gen ())) false (increment height)),
             (increment height), (goal (int_gen ()) false (increment inv))) : 
  int rbtree)
