let[@library] nil = (len v 0 : [%v: int list]) [@under]

let[@library] cons =
  let h = (true : [%v: int]) [@over] in
  let s = (true : [%v: int]) [@over] in
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

let[@library] leaf = (len v 0 : [%v: int tree])

let[@library] node =
  let root = (true : [%v: int]) [@over] in
  let sizel = (v >= 0 : [%v: int]) [@over] in
  let _ =
    (fun (u : [%forall: int]) ->
       implies (mem v u) (root - sizel < u && u < root)
       && sorted v && len v sizel
      : [%v: int tree])
      [@under]
  in
  let sizer = (v >= 0 : [%v: int]) [@over] in
  let _ =
    (fun (u : [%forall: int]) ->
       implies (mem v u) (root < u && u < root + sizer)
       && sorted v && len v sizer
      : [%v: int tree])
      [@under]
  in
  (fun (u : [%forall: int]) ->
     implies (mem v u) (root - sizel < u && u < root + sizer) && sorted v
    : [%v: int tree])
    [@under]
