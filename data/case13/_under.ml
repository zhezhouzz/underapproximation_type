let foo =
  let x = (v >= 0 : [%v: int]) in
  let y = (v >= 0 : [%v: int]) in
  let z = (v >= 0 : [%v: int]) in
  (v >= 1 : [%v: int])

let foo =
  let x = (v >= 0 : [%v: int]) in
  let y = (v >= 0 : [%v: int]) in
  let z = (v >= 0 : [%v: int]) in
  (v >= 1 + y : [%v: int])

let foo =
  let x = (v >= 0 : [%v: int]) in
  let y = (v >= x : [%v: int]) in
  let z = (v >= 0 : [%v: int]) in
  (v >= 1 + y : [%v: int])

let foo =
  let x = (v >= 0 : [%v: int]) in
  let y = (v >= x : [%v: int]) in
  let z = (v >= 0 : [%v: int]) in
  (v >= 1 + y + x : [%v: int])
