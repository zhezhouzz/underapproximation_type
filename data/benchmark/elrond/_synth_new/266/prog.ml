let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then goal (subs x0) x0
   else x0 +:: (goal size1 (subs size1)) : int ulist)