let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then goal (subs x0) (subs size1)
   else (subs x0) +:: (goal (subs size) (subs size1)) : int ulist)