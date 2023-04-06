let rec goal (inv : int) (c : bool) (height : int) =
  (if c
   then
     Rbtnode
       (true, (goal inv c inv), (increment inv),
         (goal inv true (increment r)))
   else
     if lt_eq_one height
     then goal (increment inv) false (increment inv)
     else goal (increment inv) false (increment (int_gen ())) : int rbtree)
