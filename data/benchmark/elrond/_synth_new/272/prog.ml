let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then (subs size) +:: (goal (subs size) (subs size))
   else x0 +:: (goal (subs x0) (subs size1)) : int ulist)
