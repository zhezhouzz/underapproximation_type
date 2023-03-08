let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then (subs size) +:: (goal (subs size1) (subs size1))
   else x0 +:: (goal (subs size1) (subs size)) : int ulist)
