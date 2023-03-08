let rec goal (size : int) (x0 : int) =
  (if sizecheck x0 then goal (subs x0) x0 else goal (subs size) (subs x0) : 
  int ulist)
