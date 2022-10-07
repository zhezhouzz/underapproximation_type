let rec list_gen (size : int) : int list =
  let (b : bool) = size == 0 in
  if b then []
  else
    let (size2 : int) = size - 1 in
    let (l : int list) = list_gen size2 in
    let (x : int) = int_gen () in
    let (l2 : int list) = x :: l in
    l2
