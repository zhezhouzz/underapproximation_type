let rec bst_gen_v1 (lo : int) (hi : int) : int list =
  let (b : bool) = size == 0 in
  if b then []
  else
    let (y : int) = int_gen () in
    if x <= y then
      let (size2 : int) = size - 1 in
      let (l : int list) = sorted_list_gen size2 y in
      let (l2 : int list) = x :: l in
      l2
    else Exn
