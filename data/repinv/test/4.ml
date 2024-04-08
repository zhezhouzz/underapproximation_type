let foo (m : int) (n : int) : int = if n > 0 then 1 else 2

let[@assert] foo =
  let m = (true : [%v: int]) [@over] in
  let n = (true : [%v: int]) [@under] in
  (v == 1 : [%v: int]) [@under]
