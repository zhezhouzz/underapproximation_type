let rec sorted_list_gen (size : int) (x : int) : int list =
  let (b : bool) = size == 0 in
  if b then []
  else
    let (y : int) = int_gen () in
    if x <= y then
      let (l : int list) = sorted_list_gen (int_gen ()) y in
      let (l2 : int list) = y :: l in
      l2
    else Exn

let[@assert] sorted_list_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  let x = (true : [%v: int]) [@over] in
  (len v s && sorted v && fun (u : int) -> (hd v u) #==> (x <= u)
    : [%v: int list])
    [@under]
