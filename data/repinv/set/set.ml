let empty = ([] : int list)

let rec insert (x : int) (s : int list) : int list =
  match s with
  | [] -> [ x ]
  | h :: t -> if x == h then s else if x < h then x :: s else h :: insert x t

let rec delete (x : int) (s : int list) : int list =
  match s with [] -> [] | h :: t -> if x == h then s else h :: delete x t

let[@assert] insert =
  let x = (true : [%v: int]) in
  let s = (uniq v : [%v: int list]) in
  ((not (uniq v)) && list_mem s x : [%v: int list]) [@over]
