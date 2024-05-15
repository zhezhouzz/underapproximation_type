let rec insert (x : elem) (s : elem list) : elem list =
  match s with
  | [] -> [ x ]
  | h :: t ->
      if elem_eq x h then h :: t
      else if elem_lt x h then x :: h :: t
      else h :: insert x t

let[@assert] insert =
  let (i [@ghost]) = (0 < v : [%v: int]) [@over] in
  let x = (true : [%v: elem]) [@under] in
  let s = (sorted v && len v (i - 1) : [%v: elem list]) [@under] in
  (sorted v && len v i : [%v: elem list]) [@under]
