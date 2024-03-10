let rec goal (size : int) (x0 : int) : int list =
  if sizecheck size then [] else [ x0 ]

let[@assert] goal =
  let s = (0 <= v : [%v: int]) [@over] in
  let x = (true : [%v: int]) [@over] in
  (len v s && sorted v && fun (u : int) -> (hd v u) #==> (x <= u)
    : [%v: int list])
    [@under]
