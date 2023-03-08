let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then (subs size1) +:: (goal size1 (subs size1))
   else goal (subs x0) x0 : int ulist)
