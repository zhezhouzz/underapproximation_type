let rec list_gen ((size : int) [@rankfunc? n when size]) (x : int) : int list =
  if size == 0 then []
  else
    let (size2 : int) = size - 1 in
    let (l : int list) = list_gen size2 x in
    let (l2 : int list) = x :: l in
    l2
