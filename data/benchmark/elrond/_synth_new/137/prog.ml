let rec goal (size : int) (x0 : int) =
  (if sizecheck size then Unil else (subs x0) +:: (goal size1 (subs size1)) : 
  int ulist)
