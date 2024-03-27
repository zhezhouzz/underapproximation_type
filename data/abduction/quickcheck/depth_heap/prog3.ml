let rec depth_heap_gen (d : int) (mx : int) : int tree =
  if d == 0 then Leaf
  else if bool_gen () then Leaf
  else
    let (n : int) = int_gen () in
    if n < mx then Err else Err

let[@assert] depth_heap_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  let mx = (true : [%v: int]) [@over] in
  (heap v
   && (fun ((u [@exists]) : int) -> depth v u && u <= s)
   && fun (u : int) -> implies (root v u) (u < mx)
    : [%v: int tree])
    [@under]
