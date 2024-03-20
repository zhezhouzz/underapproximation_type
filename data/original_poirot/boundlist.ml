let rec bound_list_gen (size : int) (x : int) : int list =
  let (b : bool) = size == 0 in
  if b then []
  else
    let (y : int) = int_gen () in
    if x <= y then
      let (size2 : int) = size - 1 in
      let (l : int list) = bound_list_gen size2 x in
      let (l2 : int list) = y :: l in
      l2
    else Exn

let[@assert] bound_list_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  let x = (true : [%v: int]) [@over] in
  (len v s && fun (u : int) -> implies (list_mem v u) (x <= u)
    : [%v: int list])
    [@under]
