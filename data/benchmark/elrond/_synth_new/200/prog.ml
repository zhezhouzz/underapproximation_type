let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then (subs size) +:: (goal (subs x0) (subs size1))
   else (subs size) +:: (goal (subs size) x0) : int ulist)
