let rec goal (inv : int) (c : bool) (height : int) =
  (if lt_eq_one height
   then
     Rbtnode
       (true, (goal (int_gen ()) false inv), (increment inv),
         (goal (increment inv) false height))
   else
     Rbtnode
       (true, (goal (increment height) c height), (int_gen ()),
         (goal (int_gen ()) c (increment inv))) : int rbtree)
