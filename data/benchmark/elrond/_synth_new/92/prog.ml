let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then goal (subs size) (subs size1)
   else goal (subs size) x0 : int ulist)