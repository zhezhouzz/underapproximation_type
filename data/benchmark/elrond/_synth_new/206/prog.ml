let rec goal (size : int) (x0 : int) =
  (if sizecheck x0
   then goal (subs size) (subs size)
   else (subs size) +:: Unil : int ulist)
