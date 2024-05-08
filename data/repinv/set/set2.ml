let rec insert (s : int list) (x : int) : int list =
  match s with [] -> [ x ] | h :: t -> if x == h then s else x :: insert t h

let[@assert] insert (i : int) =
  let s = (uniq v && len v i : [%v: int list]) [@under] in
  let x = (not (list_mem s v) : [%v: int]) [@under] in
  (uniq v
   && len v (i + 1)
   && fun (u : int) -> iff (list_mem v u) (list_mem s u || u == x)
    : [%v: int list])
    [@under]
