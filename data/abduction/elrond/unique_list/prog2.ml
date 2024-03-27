let rec unique_list_gen (s : int) : int list =
  if sizecheck s then []
  else
    let (l : int list) = unique_list_gen (subs s) in
    let (x : int) = int_gen () in
    if list_mem l x then Err else Err

let[@assert] unique_list_gen =
  let s = (v >= 0 : [%v: int]) [@over] in
  (len v s && uniq v : [%v: int list]) [@under]
