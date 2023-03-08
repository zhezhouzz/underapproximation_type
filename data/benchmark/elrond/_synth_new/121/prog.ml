let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then goal (subs size) (subs x0)
   else goal (subs x0) (subs size1) : int ulist)
