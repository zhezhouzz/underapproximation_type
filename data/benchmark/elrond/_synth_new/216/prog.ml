let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then goal (subs size) (subs x0)
   else (subs size1) +:: (goal (subs x0) (subs size)) : int ulist)