let[@library] nil = (len v 0 : [%v: int list]) [@under]

let[@library] cons =
  let s = (true : [%v: int]) [@over] in
  let h = (true : [%v: int]) [@over] in
  let _ =
    (len v s && fun (u : [%forall: int]) (w : [%forall: int]) ->
     implies (mem v u) (h <= u) && implies (ord v u w) (u <= w)
      : [%v: int list])
      [@under]
  in
  (fun (u : [%forall: int]) (w : [%forall: int]) ->
     implies (u == s + 1) (len v u)
     && implies (mem v u) (h <= u)
     && implies (ord v u w) (u <= w)
    : [%v: int list])
    [@under]

let[@library] leaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int tree]) [@under]

let[@library] node =
  let lo = (true : [%v: int]) [@over] in
  let hi = (true : [%v: int]) [@over] in
  let root = (lo < v && v < hi : [%v: int]) [@over] in
  let _ =
    (fun (u : [%forall: int]) (w : [%forall: int]) ->
       implies (mem v u) (lo < u && u < root) && implies (ord v u w) (u < w)
      : [%v: int tree])
      [@under]
  in
  let _ =
    (fun (u : [%forall: int]) (w : [%forall: int]) ->
       implies (mem v u) (root < u && u < hi) && implies (ord v u w) (u < w)
      : [%v: int tree])
      [@under]
  in
  (fun (u : [%forall: int]) (w : [%forall: int]) ->
     hd v root
     && implies (mem v u) (lo < u && u < hi)
     && implies (ord v u w) (u < w)
    : [%v: int tree])
    [@under]
