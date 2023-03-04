let[@library] unil = (len v 0 : [%v: int ulist]) [@under]

let[@library] ucons =
  let h = (true : [%v: int]) [@over] in
  let s = (v >= 0 : [%v: int]) [@over] in
  let _ =
    (len v s && fun (u : [%forall: int]) -> implies (mem v u) (u == h)
      : [%v: int ulist])
      [@under]
  in
  (fun (u : [%forall: int]) ->
     implies (u == s + 1) (len v u) && implies (mem v u) (u == h)
    : [%v: int ulist])
    [@under]
