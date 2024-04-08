let foo (n : int) (m : int) : int = if n > 0 then 1 else 2

let[@assert] foo =
  let n = (true : [%v: int]) [@under] in
  let m = (true : [%v: int]) [@over] in
  (v == 1 : [%v: int]) [@under]
