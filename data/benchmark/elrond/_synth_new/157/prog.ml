let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then (subs size1) +:: (goal (subs size1) (subs x0))
   else (subs size1) +:: (goal (subs size1) x0) : int ulist)