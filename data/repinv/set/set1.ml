let rec insert (x : int) (s : int list) : int list =
  match s with [] -> [ x ] | h :: t -> Err

let[@assert] insert =
  let x = (true : [%v: int]) in
  let s = (uniq v : [%v: int list]) in
  ((not (uniq v)) && list_mem s x : [%v: int list]) [@over]
