let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then goal (subs size) (subs size)
   else (subs size1) +:: (goal (subs size1) x0) : int ulist)
