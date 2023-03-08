let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then goal (subs size) (subs x0)
   else x0 +:: (goal size1 (subs size1)) : int ulist)
