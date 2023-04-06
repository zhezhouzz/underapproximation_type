let rec goal (inv : int) (c : bool) (height : int) =
  (if sizecheck height
   then
     (if c then Rbtleaf else Rbtnode (true, Rbtleaf, (int_gen ()), Rbtleaf))
   else
     if c
     then
       Rbtnode
         (false, (goal (subs inv) false (subs height)), (int_gen ()),
           (goal (subs inv) false (subs height)))
     else
       if bool_gen ()
       then
         Rbtnode
           (true, (goal (subs inv) true height), (int_gen ()),
             (goal (subs inv) true height))
       else
         Rbtnode
           (false, (goal (subs (subs inv)) false (subs height)),
             (int_gen ()), (goal (subs (subs inv)) false (subs height))) : 
  int rbtree)
