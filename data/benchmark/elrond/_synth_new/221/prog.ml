let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then (subs x0) +:: (goal (subs size) x0)
   else (subs x0) +:: (goal (subs size) (subs size1)) : int ulist)