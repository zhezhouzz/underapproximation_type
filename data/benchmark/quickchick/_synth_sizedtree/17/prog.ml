let rec goal (s0 : int) =
  (if sizecheck s0
   then Leaf
   else
     if bool_gen ()
     then goal (subs s0)
     else Node ((int_gen ()), (goal (subs s0)), (goal (subs s0))) : int tree)
