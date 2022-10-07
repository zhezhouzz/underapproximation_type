let nat_gen =
  let (dummy : [%under: (unit -> int) -> int]) =
    let (dummy : [%under: unit -> int]) =
      let (x : [%over: unit]) = (true : [%v: unit]) in
      (true : [%v: int])
    in
    (v >= 1 : [%v: int])
  in
  let (dummy : [%under: unit -> int]) =
    let (x : [%over: unit]) = (true : [%v: unit]) in
    (true : [%v: int])
  in
  (v >= 2 : [%v: int])
