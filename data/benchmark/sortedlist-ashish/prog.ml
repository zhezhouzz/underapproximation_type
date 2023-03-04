let rec sorted_list_gen (size : int) (x : int) : int list =
  let (b : bool) = size == 0 in
  if b then []
  else
    let (y : int) = lower_bound_int_gen x in
    let (size2 : int) = size - 1 in
    let (l : int list) = sorted_list_gen size2 y in
    let (z : int) = lower_bound_int_gen x in
    let (l2 : int list) = z :: l in
    l2
