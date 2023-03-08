let rec goal (size : int) (x0 : int) =
  (if sizecheck size
   then Unil
   else (subs x0) +:: (goal (subs size) (subs size1)) : int ulist)
