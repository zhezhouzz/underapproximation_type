let[@library] int_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: int])

let[@library] bool_gen =
  let (dummy : [%over: unit]) = (true : [%v: unit]) in
  (true : [%v: bool])

let sized_list_gen =
  let (s : [%over: int]) = (0 <= v : [%v: int]) in
  (fun (u : [%forall: int]) -> implies (len v u) (0 <= u && u <= s)
    : [%v: int list])
