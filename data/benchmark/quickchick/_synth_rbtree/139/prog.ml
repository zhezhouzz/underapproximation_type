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
         (true, (goal (increment height) c height), (increment (int_gen ())),
           (goal (int_gen ()) c height))
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
             (true, (goal (increment inv) false (increment inv)),
               (increment inv), (goal r c inv))
         else
           if lt_eq_one height
           then goal (increment inv) true (increment inv)
           else
             Rbtnode
               (true, (goal (increment inv) true (increment inv)),
                 (increment height), (goal (increment inv) false height)) : 
  int rbtree)
