let[@notation] t_y = (v > 0 : [%v: int])

let foo =
  let x = (v > 0 : [%v: int]) in
  (v > 1 : [%v: int])
