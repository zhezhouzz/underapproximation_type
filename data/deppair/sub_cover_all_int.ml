let sub_cover_all_int (a : int) (b : int) : int = a - b

let[@assert] sub_cover_all_int ?l:(a = (0 <= v : [%v: int])) =
  let b = (true : [%v: int]) in
  (true : [%v: int])
