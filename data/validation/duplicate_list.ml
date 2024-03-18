let rec duplicate_list_gen (s : int) (x : int) : int list =
  if s == 0 then [] else x :: duplicate_list_gen (s - 1) x

let[@assert] duplicate_list_gen =
  let s = (v >= 0 : [%v: int]) [@over] in
  let x = (true : [%v: int]) [@over] in
  (len v s && fun (u : int) -> (list_mem v u) #==> (u == x)
    : [%v: int list])
    [@under]
