let rec duplicate_list_gen (s : int) (x : int) : int list =
  if sizecheck s then [] else Err

let[@assert] duplicate_list_gen =
  let s = (v >= 0 : [%v: int]) [@over] in
  let x = (true : [%v: int]) [@over] in
  (len v s && fun (u : int) -> (list_mem v u) #==> (u == x)
    : [%v: int list])
    [@under]
