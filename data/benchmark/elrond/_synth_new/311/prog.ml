let rec goal (size : int) (x0 : int) =
  (if sizecheck size then Unil else x0 +:: (goal (subs size1) (subs size1)) : 
  int ulist)
