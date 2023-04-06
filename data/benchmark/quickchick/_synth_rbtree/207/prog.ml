let rec goal (inv : int) (c : bool) (height : int) =
  (if lt_eq_one inv
   then
     Rbtnode
       (true, (goal (increment inv) false height), inv,
         (goal (int_gen ()) c (increment (int_gen ()))))
   else goal (increment (int_gen ())) true (increment inv) : int rbtree)
