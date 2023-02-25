let[@library] nil = (len v 0 : [%v: int list]) [@under]

let[@library] cons =
  let h = (true : [%v: int]) [@over] in
  let s = (v >= 0 : [%v: int]) [@over] in
  let _ =
    (len v s && fun (u : [%forall: int]) -> implies (mem v u) (u == h)
      : [%v: int list])
      [@under]
  in
  (fun (u : [%forall: int]) ->
     implies (u == s + 1) (len v u) && implies (mem v u) (u == h)
    : [%v: int list])
    [@under]
