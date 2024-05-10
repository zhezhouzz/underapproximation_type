let empty = []

let[@assert] empty =
  let (i [@ghost]) = (0 == v : [%v: int]) [@over] in
  (uniq v && len v i : [%v: int list]) [@under]
