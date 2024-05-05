let rec foo (n : int) : int = if n > 0 then foo (n - 1) else n

let[@assert] foo (i : int) =
  let n = (v == i : [%v: int]) [@under] in
  (v == 0 : [%v: int]) [@under]
