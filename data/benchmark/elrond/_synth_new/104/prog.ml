let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then (subs size1) +:: (goal (subs x0) (subs size))
   else goal (subs x0) (subs size) : int ulist)
