let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then goal (subs x0) (subs x0)
   else x0 +:: (goal (subs size1) (subs size)) : int ulist)