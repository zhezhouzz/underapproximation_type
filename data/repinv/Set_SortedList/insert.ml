let rec insert (x : int) (s : int list) : int list =
  match s with
  | [] -> [ x ]
  | h :: t ->
      if x == h then h :: t else if x < h then x :: h :: t else h :: insert x t

let[@assert] insert =
  let (i [@ghost]) = (0 < v : [%v: int]) [@over] in
  let x = (true : [%v: int]) [@under] in
  let s = (sorted v && len v (i - 1) : [%v: int list]) [@under] in
  (sorted v && len v i : [%v: int list]) [@under]
