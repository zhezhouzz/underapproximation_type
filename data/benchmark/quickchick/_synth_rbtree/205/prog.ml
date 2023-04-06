let rec goal (inv : int) (c : bool) (height : int) =
  (if lt_eq_one inv
   then
     Rbtnode
       (true, (goal (int_gen ()) c (increment (int_gen ()))),
         (increment inv), (goal inv true inv))
   else goal (increment (int_gen ())) true (increment inv) : int rbtree)
