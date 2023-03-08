let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then (subs size1) +:: (goal (subs size1) (subs x0))
   else goal (subs size) (subs size1) : int ulist)
