let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then goal (subs x0) (subs size)
   else x0 +:: (goal (subs x0) (subs size)) : int ulist)
