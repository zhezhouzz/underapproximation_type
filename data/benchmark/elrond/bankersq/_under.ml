let[@library] int_range =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (a <= v : [%v: int]) in
  (a <= v && v <= b : [%v: int])

let[@library] stream_gen =
  let (a : [%over: int]) = (true : [%v: int]) in
  (fun (u : [%forall: int]) -> implies (0 <= u && u <= a) (len v u)
    : [%v: int stream])

let bankersq_gen =
  let (s : [%over: int]) = (v >= 0 : [%v: int]) in
  (len v s : [%v: int bankersq])
