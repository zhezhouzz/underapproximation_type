let rec goal (inv : int) (c : bool) (height : int) =
  (if lt_eq_one height
   then
     Rbtnode
       (true, (goal (increment inv) false height), (increment (int_gen ())),
         (goal (increment inv) false height))
   else
     Rbtnode
       (true, (goal inv true inv), (int_gen ()),
         (goal (int_gen ()) false (increment height))) : int rbtree)
