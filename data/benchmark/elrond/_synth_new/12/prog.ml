let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then (subs size1) +:: (goal (subs x0) (subs size))
   else (subs x0) +:: (goal (subs x0) (subs x0)) : int ulist)
