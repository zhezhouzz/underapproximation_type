let bar =
  let (dummy : [%under: unit -> int]) =
    let (x : [%over: unit]) = (true : [%v: unit]) in
    (true : [%v: int])
  in
  let (dummy : [%under: int -> int]) =
    let (x : [%over: int]) = (v > 0 : [%v: int]) in
    (v == x + 1 : [%v: int])
  in
  let (dummy : [%under: int -> int]) =
    let (x : [%over: int]) = (true : [%v: int]) in
    (v == x + 1 : [%v: int])
  in
  (v >= 2 : [%v: int])
(* even  *)

let bar =
  let (dummy : [%under: unit -> int]) =
    let (x : [%over: unit]) = (true : [%v: unit]) in
    (true : [%v: int])
  in
  let (dummy : [%under: int -> int]) =
    let (dummy : [%under: int]) = (v > 0 : [%v: int]) in
    (v > 1 : [%v: int])
  in
  let (dummy : [%under: int -> int]) =
    let (x : [%over: int]) = (true : [%v: int]) in
    (v == x + 1 : [%v: int])
  in
  (v == 4 : [%v: int])
