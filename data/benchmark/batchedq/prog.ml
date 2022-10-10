let batchedq_gen (sizer : int) : int batchedq =
  let (sizel : int) = int_range 0 sizer in
  let (l1 : int list) = list_gen sizel in
  let (l2 : int list) = list_gen sizer in
  Batchedq (l1, l2)
