let[@assert] rty1 =
  let x = (v < 4 : [%v: int]) [@under] in
  (v == 6 - x : [%v: int]) [@under]

let[@assert] rty2 =
  let x = (v < 6 : [%v: int]) [@over] in
  (v > 3 : [%v: int]) [@over]
