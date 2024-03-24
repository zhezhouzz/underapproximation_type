let rec unique_list_gen (s : int) : int list =
  if s == 0 then []
  else
    let (l : int list) = unique_list_gen (s - 1) in
    let (x : int) = int_gen () in
    if list_mem l x then Err else x :: l

let[@assert] unique_list_gen =
  let s = (v >= 0 : [%v: int]) [@over] in
  (len v s && uniq v : [%v: int list]) [@under]
