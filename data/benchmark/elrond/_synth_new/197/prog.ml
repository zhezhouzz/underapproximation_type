let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then (subs size) +:: (goal (subs size) (subs size))
   else x0 +:: (goal size1 (subs x0)) : int ulist)
