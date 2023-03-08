let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then goal (subs x0) (subs size1)
   else x0 +:: (goal (subs size) (subs size)) : int ulist)
